{-# language BangPatterns #-}
{-# language MagicHash #-}
{-# language LambdaCase #-}
{-# language ScopedTypeVariables #-}

module Logger
  ( -- * Types
    Logger
    -- * Open
  , fromHandle
  , fromFd
    -- * Log
  , builder
  , boundedBuilder
  , chunks
  , bytes
  , byteArray
  , cstring
  , cstring#
    -- * Flush
  , flush
  ) where

import Control.Concurrent (MVar,threadWaitWrite,putMVar,newMVar,tryTakeMVar,rtsSupportsBoundThreads)
import Control.Exception (SomeException,toException,onException,mask)
import Control.Monad (when)
import Data.Bits ((.&.))
import Data.Bytes.Builder (Builder)
import Data.Bytes.Chunks (Chunks)
import Data.Bytes.Types (Bytes(Bytes))
import Data.IORef (IORef,atomicModifyIORef',newIORef)
import Data.Primitive (MutablePrimArray,ByteArray)
import Foreign.C.Error (eINTR,eWOULDBLOCK,eAGAIN,eBADF)
import Foreign.C.String (CString)
import GHC.Exts (Ptr(Ptr),RealWorld,Addr#)
import GHC.IO (IO(IO))
import Posix.File (uninterruptibleWriteByteArray,uninterruptibleGetStatusFlags)
import System.IO (Handle)
import System.Posix.Types (Fd(Fd))

import qualified Arithmetic.Types as Arithmetic
import qualified Data.Bytes.Builder as Builder
import qualified Data.Bytes.Builder.Bounded as BB
import qualified Data.Bytes as Bytes
import qualified Data.Bytes.Chunks as Chunks
import qualified Data.Primitive as PM
import qualified GHC.Exts as Exts
import qualified GHC.IO.FD as FD
import qualified GHC.IO.Handle.FD as FD
import qualified Posix.File as File

-- | An output channel for logs. The intent is that there is only
-- one logger per application, opened at load time and shared across
-- all threads.
data Logger = Logger
  {-# UNPACK #-} !Fd
  -- ^ Output file descriptor, often stdout or stderr. Must be
  -- in nonblocking mode. This is checked when the logger is opened.
  {-# UNPACK #-} !Int
  -- ^ The number 1 if file descriptor is nonblocking. The number 0
  -- if it is blocking. Notably, stderr will almost certainly be in
  -- blocking mode.
  {-# UNPACK #-} !(MVar ())
  -- ^ Lock, only used when flushing.
  {-# UNPACK #-} !(IORef Chunks)
  -- ^ Chunks to be written out. This is only ever accessed by
  -- thread-safe atomicModify functions.
  {-# UNPACK #-} !(MutablePrimArray RealWorld Int)
  -- ^ Singleton array with a counter. This is used as a heuristic
  -- for when to flush. This is accessed in a non-thread-safe way
  -- where updates can be overridden. This is fine since logs will
  -- eventually get flushed anyway. Note that this is based on a
  -- number of units atomically written to the logger, not based
  -- on a byte count. Intuitively, it is deeply wrong to decide
  -- when to flush like this, but small-bytearray-builder is not
  -- able to provide the number of bytes that result from running
  -- a builder, so we use this hack instead.

-- | Convert a 'Handle' to a logger by extracting its file descriptor.
-- Warning: the caller must ensure that the handle does not get garbage
-- collected. This is very dangerous.
fromHandle :: Handle -> IO Logger
fromHandle h = do
  FD.FD{FD.fdFD=fd} <- FD.handleToFd h
  fromFd (Fd fd)

-- | Convert a file descriptor to a logger.
fromFd :: Fd -> IO Logger
fromFd !fd = do
  when (not rtsSupportsBoundThreads) (die threadedRuntimeRequired)
  status <- uninterruptibleGetStatusFlags fd >>= \case
    Left _ -> die flagsFailure
    Right status -> pure status
  when (not (File.isWriteOnly status || File.isReadWrite status))
    (die statusWriteFailure)
  let !nonblocking = if File.nonblocking .&. status == File.nonblocking
        then 1
        else 0
  !lock <- newMVar ()
  !ref <- newIORef Chunks.ChunksNil
  !counterRef <- PM.newPrimArray 1
  PM.writePrimArray counterRef 0 0
  pure $! Logger fd nonblocking lock ref counterRef

threshold :: Int
threshold = 32

-- | Log a @NUL@-terminated C string.
cstring :: Logger -> CString -> IO ()
cstring g str = builder g (Builder.cstring str)

-- | Log a @NUL@-terminated C string. Takes the unboxed equivalent of @CString@.
-- This improves legibility in a common case:
--
-- > cstring logger (Ptr "Initializing..."#)
--
-- This can be written more succinctly as:
--
-- > cstring# logger "Initializing..."#
cstring# :: Logger -> Addr# -> IO ()
cstring# g str = builder g (Builder.cstring (Ptr str))

chunks :: Logger -> Chunks -> IO ()
{-# noinline chunks #-}
chunks logger@(Logger _ _ _ ref counterRef) ch = do
  atomicModifyIORef' ref
    (\cs0 ->
      let !cs1 = Chunks.reverseOnto cs0 ch
       in (cs1,())
    )
  !counter <- bumpCounter counterRef
  when (counter >= threshold) $ do
    -- Reset the counter
    PM.writePrimArray counterRef 0 0
    flush logger

-- | Log the chunks that result from executing the byte builder.
builder :: Logger -> Builder -> IO ()
builder logger@(Logger _ _ _ ref counterRef) bldr = do
  atomicModifyIORef' ref
    (\cs0 ->
      -- Why use reversedOnto? The chunks are arranged backwards.
      -- in the buffer. When we flush, they get reversed and end
      -- up in the correct order.
      let !cs1 = Builder.reversedOnto 240 bldr cs0
       in (cs1,())
    )
  !counter <- bumpCounter counterRef
  when (counter >= threshold) $ do
    -- Reset the counter
    PM.writePrimArray counterRef 0 0
    flush logger

-- | Log the unsliced byte array that results from executing
-- the bounded builder.
boundedBuilder :: Logger -> Arithmetic.Nat n -> BB.Builder n -> IO ()
boundedBuilder logger n b = byteArray logger (BB.run n b)

-- | Log a byte sequence.
bytes :: Logger -> Bytes -> IO ()
bytes logger@(Logger _ _ _ ref counterRef) !b = do
  atomicModifyIORef' ref
    (\cs0 -> let !cs1 = Chunks.ChunksCons b cs0 in (cs1,()))
  !counter <- bumpCounter counterRef
  when (counter >= threshold) (flush logger)

-- | Log an unsliced byte array.
byteArray :: Logger -> ByteArray -> IO ()
byteArray logger = bytes logger . Bytes.fromByteArray

bumpCounter :: MutablePrimArray RealWorld Int -> IO Int
bumpCounter arr = do
  counter <- PM.readPrimArray arr 0
  let counter' = counter + 1
  PM.writePrimArray arr 0 counter'
  pure counter'

-- | Flush any pending logs out to the file descriptor.
flush :: Logger -> IO ()
{-# noinline flush #-}
flush (Logger fd nonblocking lock ref _) = mask $ \restore -> tryTakeMVar lock >>= \case
  -- Try to take the lock. This cannot be interrupted by async exceptions.
  -- If we cannot take the lock immidiately, someone else is already flushing
  -- logs, so we just give up.
  Nothing -> pure ()
  Just (_ :: ()) -> do
    -- Atomically remove all logs from the batch. GHC guarantees
    -- that this cannot be interrupted by async exceptions
    -- inside of mask.
    yanked <- atomicModifyIORef' ref (\cs -> (Chunks.ChunksNil,cs))
    -- When cleaning up after an exception, we put all the logs
    -- back. This means that logs may end up duplicated. That is
    -- in addition to the inevitable chance of a torn log that
    -- was being written out when throwTo delivered the exception.
    -- The decision about the ordering of @cs@ and @yanked@ during
    -- cleanup is arbitrary.
    onException
      (restore (action yanked))
      (do atomicModifyIORef' ref (\cs -> (cs <> yanked,()))
          putMVar lock ()
      )
    putMVar lock ()
    where
    action yanked = case nonblocking of
      1 -> writeNonblocking fd yanked
      _ -> writeBlocking fd yanked

writeNonblocking :: Fd -> Chunks -> IO ()
writeNonblocking !fd yanked = go off0 len0
  -- TODO: Use writev so that we do not have to allocate
  -- memory to concatenate the chunks. This would need to
  -- be added to posix-api.
  where
  Bytes arr off0 len0 = Chunks.concat (Chunks.reverse yanked)
  go off len = do
    -- Remember, the file descriptor is known to be in non-blocking mode,
    -- so uninterruptible is fine.
    uninterruptibleWriteByteArray fd arr off (fromIntegral len) >>= \case
      Left err
        | err == eWOULDBLOCK || err == eAGAIN -> do
            threadWaitWrite fd
            go off len
        | err == eINTR -> go off len
        | err == eBADF -> die flushBadFdFailure
        | otherwise -> die flushFailure
      Right writtenC -> do
        let written = fromIntegral writtenC :: Int
        if written == len
          then pure ()
          else go (off + written) (len - written)

writeBlocking :: Fd -> Chunks -> IO ()
writeBlocking !fd yanked = go off0 len0
  -- Note: we concatenate the chunks into pinned memory. Pinned memory
  -- is required since we use the safe FFI.
  where
  Bytes arr off0 len0 = Chunks.concatPinned (Chunks.reverse yanked)
  go off len = do
    -- The file descriptor is known to be in blocking mode,
    -- so we do not bother with the event manager.
    File.writeByteArray fd arr off (fromIntegral len) >>= \case
      Left _ -> die flushFailure
      Right writtenC -> do
        let written = fromIntegral writtenC :: Int
        if written == len
          then pure ()
          else go (off + written) (len - written)

die :: SomeException -> IO a
{-# inline die #-}
die e = IO (Exts.raiseIO# e)

flagsFailure :: SomeException
{-# noinline flagsFailure #-}
flagsFailure = toException
  (userError "Logger: fcntl failed")

statusWriteFailure :: SomeException
{-# noinline statusWriteFailure #-}
statusWriteFailure = toException
  (userError "Logger: descriptor must have O_WRONLY or O_RDWR")

flushFailure :: SomeException
{-# noinline flushFailure #-}
flushFailure = toException (userError "Logger: flush encountered unknown error")

flushBadFdFailure :: SomeException
{-# noinline flushBadFdFailure #-}
flushBadFdFailure = toException (userError "Logger: EBADF while flushing")

threadedRuntimeRequired :: SomeException
{-# noinline threadedRuntimeRequired #-}
threadedRuntimeRequired = toException (userError "Logger: threaded runtime required")
