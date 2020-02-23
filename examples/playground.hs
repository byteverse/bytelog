{-# language BangPatterns #-}
{-# language MagicHash #-}

{-# OPTIONS_GHC -O2 -Wall #-}

import Control.Concurrent (forkIO)
import Control.Concurrent.STM (TVar)
import Data.Char (ord)
import Data.Primitive (ByteArray)
import Data.Word (Word64)
import GHC.Exts (Ptr(Ptr),Char(C#),Int(I#))
import Logger (Logger)

import qualified Arithmetic.Nat as Nat
import qualified Control.Concurrent.STM as STM
import qualified Data.ByteArray.Builder as B
import qualified Data.ByteArray.Builder.Bounded as BB
import qualified Data.Bytes.Chunks as Chunks
import qualified GHC.Exts as Exts
import qualified Logger
import qualified System.IO as IO

main :: IO ()
main = do
  handle <- IO.openFile "results.txt" IO.AppendMode 
  logger <- Logger.fromHandle handle
  counter <- STM.newTVarIO 0
  let workers = 20
  let go !ix = if ix < workers
        then do
          _ <- forkIO (runWorker logger ix counter)
          go (ix + 1)
        else pure ()
  go 0
  -- let prefix = makePrefix 1000
  -- logWorkerAndUnit logger prefix 200003
  -- logWorkerAndUnit logger prefix 200004
  -- logWorkerAndUnit logger prefix 200005
  -- logLetters logger prefix
  STM.atomically $ do
    n <- STM.readTVar counter
    STM.check (n == workers)
  Logger.flush logger
  IO.hClose handle

runWorker :: Logger -> Word64 -> TVar Word64 -> IO ()
runWorker !logger !ident !counter = go 0
  where
  !prefix = makePrefix ident
  go !ix = if ix < 10000
    then do
      logWorkerAndUnit logger prefix ix
      go (ix + 1)
    else STM.atomically (STM.modifyTVar' counter (+1))

makePrefix :: Word64 -> ByteArray
makePrefix !worker = Chunks.concatU $ B.run 100 $
     B.cstring (Ptr "[Worker "# )
  <> B.word64Dec worker
  <> B.ascii ']'

logWorkerAndUnit :: Logger.Logger -> ByteArray -> Word64 -> IO ()
logWorkerAndUnit logger prefix unit = Logger.builder logger $
     B.byteArray prefix
  <> B.cstring (Ptr " Task number "# )
  <> B.word64Dec unit
  <> B.cstring (Ptr " was completed successfully.\n"# )

logLetters :: Logger.Logger -> ByteArray -> IO ()
logLetters logger prefix = Logger.builder logger $
     B.byteArray prefix
  <> B.cstring (Ptr " The letters of the alphabet, ten times each, are "# )
  <> letters10 'a'
  <> B.ascii '\n'

letters10 :: Char -> B.Builder
letters10 c = if c <= 'z'
  then B.fromBounded Nat.constant
    ( BB.ascii c `BB.append` BB.ascii c `BB.append` BB.ascii c `BB.append` BB.ascii c `BB.append`
      BB.ascii c `BB.append` BB.ascii c `BB.append` BB.ascii c `BB.append` BB.ascii c `BB.append`
      BB.ascii c `BB.append` BB.ascii c
    ) <> letters10 (unsafeChr (ord c + 1))
  else mempty

unsafeChr :: Int -> Char
unsafeChr (I# i) = C# (Exts.chr# i)
