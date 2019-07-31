{-# LANGUAGE BlockArguments   #-}
{-# LANGUAGE MagicHash        #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UnboxedTuples    #-}

module CopyBytes where

import           Data.ByteString              (ByteString)
import qualified Data.ByteString.Internal     as BI
import           Data.Char                    (intToDigit)
import           Data.Traversable
import qualified Data.Vector.Generic          as GV
import           Data.Vector.Storable         (Vector)
import qualified Data.Vector.Storable         as V
import qualified Data.Vector.Storable.Mutable as MV
import           Data.Word
import           Foreign.Storable
import           GHC.ForeignPtr               (ForeignPtr (..))
import           GHC.Ptr                      (Ptr (..))
import           Numeric                      (showIntAtBase)
import           System.IO.Unsafe

repetitionCode :: Int -> ByteString -> ByteString
repetitionCode rep = vectorToByteString . repeatCodeVect rep . byteStringToVect


byteStringToVect :: ByteString -> Vector Word8
byteStringToVect = (\(ptr, off, len) -> V.unsafeFromForeignPtr ptr off len) . BI.toForeignPtr

vectorToByteString :: Vector Word8 -> ByteString
vectorToByteString = (\(ptr, off, len) -> BI.fromForeignPtr ptr off len) . V.unsafeToForeignPtr

repeatCodeVect :: Storable a => Int -> Vector a -> Vector a
{-# NOINLINE repeatCodeVect #-}
repeatCodeVect rep vect =
  let
    ~(forPtrArr, _, len) = V.unsafeToForeignPtr vect
    ptrArr = forgetFinalizer forPtrArr
  in
    unsafePerformIO $ repeatCodeRaw rep ptrArr len


repeatCodeRaw :: Storable a => Int -> Ptr a -> Int -> IO (Vector a)
repeatCodeRaw rep srcArray srcLen =
  do
    v <- MV.new (rep * srcLen )
    _ <- for [0..srcLen-1] \ off ->
      do
        elt <- peekElemOff srcArray off
        for [0..rep - 1] \k ->
          MV.write v (rep * off + k) elt
    GV.basicUnsafeFreeze v


forgetFinalizer :: ForeignPtr a -> Ptr a
forgetFinalizer (ForeignPtr addr _) = Ptr addr


showBits :: Word8 -> String
showBits b = (showIntAtBase 2 intToDigit b "")

