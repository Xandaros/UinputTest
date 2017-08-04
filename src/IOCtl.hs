{-# LINE 1 "IOCtl.hsc" #-}
{-# LANGUAGE CPP, ForeignFunctionInterface #-}
module IOCtl where

import Data.Int
import Data.Monoid
import Data.Word
import Foreign
import Foreign.C
import Foreign.Ptr
import System.Posix.Types



foreign import ccall "ioctl" c_ioctl :: CInt -> CInt -> Ptr a -> IO CInt

foreign import ccall "write" c_write :: CInt -> Ptr InputEvent -> CInt -> IO ()

ioctl_i :: Fd -> Int -> Int -> IO CInt
ioctl_i fd req val = c_ioctl (fromIntegral fd) (fromIntegral req) (intPtrToPtr . fromIntegral $ val)

ioctl :: Fd -> Int -> Ptr a -> IO CInt
ioctl fd req val = c_ioctl (fromIntegral fd) (fromIntegral req) val

write :: Fd -> Ptr InputEvent -> Int -> IO ()
write fd ptr len = c_write (fromIntegral fd) ptr (fromIntegral len)


data InputId = InputId { bustype :: Word16
                       , vendorId :: Word16
                       , productId :: Word16
                       , version :: Word16
                       }

data UInputSetup = UInputSetup { input_id :: InputId
                               , name :: CString
                               , ff_effects_max :: Word32
                               }

data TimeVal = TimeVal { tv_sec :: Int64
                       , tv_usec :: Int64
                       }

emptyTimeVal :: TimeVal
emptyTimeVal = TimeVal 0 0

data InputEvent = InputEvent { time :: TimeVal
                             , typ :: Word16
                             , code :: Word16
                             , value :: Int32
                             }

instance Storable InputId where
    sizeOf    _ = (8)
{-# LINE 54 "IOCtl.hsc" #-}
    alignment _ = 2
{-# LINE 55 "IOCtl.hsc" #-}

    poke p v = do
        (\hsc_ptr -> pokeByteOff hsc_ptr 0) p $ bustype v
{-# LINE 58 "IOCtl.hsc" #-}
        (\hsc_ptr -> pokeByteOff hsc_ptr 2) p  $ vendorId v
{-# LINE 59 "IOCtl.hsc" #-}
        (\hsc_ptr -> pokeByteOff hsc_ptr 4) p $ productId v
{-# LINE 60 "IOCtl.hsc" #-}
        (\hsc_ptr -> pokeByteOff hsc_ptr 6) p $ version v
{-# LINE 61 "IOCtl.hsc" #-}

    peek p = InputId <$> (\hsc_ptr -> peekByteOff hsc_ptr 0) p
{-# LINE 63 "IOCtl.hsc" #-}
                     <*> (\hsc_ptr -> peekByteOff hsc_ptr 2)  p
{-# LINE 64 "IOCtl.hsc" #-}
                     <*> (\hsc_ptr -> peekByteOff hsc_ptr 4) p
{-# LINE 65 "IOCtl.hsc" #-}
                     <*> (\hsc_ptr -> peekByteOff hsc_ptr 6) p
{-# LINE 66 "IOCtl.hsc" #-}

instance Storable UInputSetup where
    sizeOf    _ = (92)
{-# LINE 69 "IOCtl.hsc" #-}
    alignment _ = 4
{-# LINE 70 "IOCtl.hsc" #-}

    poke p v = do
        (\hsc_ptr -> pokeByteOff hsc_ptr 0) p $ input_id v
{-# LINE 73 "IOCtl.hsc" #-}
        (\hsc_ptr -> pokeByteOff hsc_ptr 8) p $ name v
{-# LINE 74 "IOCtl.hsc" #-}
        (\hsc_ptr -> pokeByteOff hsc_ptr 88) p $ ff_effects_max v
{-# LINE 75 "IOCtl.hsc" #-}

    peek p = pure UInputSetup
                 <*> (\hsc_ptr -> peekByteOff hsc_ptr 0) p
{-# LINE 78 "IOCtl.hsc" #-}
                 <*> (\hsc_ptr -> peekByteOff hsc_ptr 8) p
{-# LINE 79 "IOCtl.hsc" #-}
                 <*> (\hsc_ptr -> peekByteOff hsc_ptr 88) p
{-# LINE 80 "IOCtl.hsc" #-}

instance Storable TimeVal where
    sizeOf    _ = (16)
{-# LINE 83 "IOCtl.hsc" #-}
    alignment _ = 8
{-# LINE 84 "IOCtl.hsc" #-}

    poke p v = do
        (\hsc_ptr -> pokeByteOff hsc_ptr 0) p $ tv_sec v
{-# LINE 87 "IOCtl.hsc" #-}
        (\hsc_ptr -> pokeByteOff hsc_ptr 8) p $ tv_usec v
{-# LINE 88 "IOCtl.hsc" #-}

    peek p = TimeVal <$> (\hsc_ptr -> peekByteOff hsc_ptr 0) p
{-# LINE 90 "IOCtl.hsc" #-}
                     <*> (\hsc_ptr -> peekByteOff hsc_ptr 8) p
{-# LINE 91 "IOCtl.hsc" #-}

instance Storable InputEvent where
    sizeOf    _ = (24)
{-# LINE 94 "IOCtl.hsc" #-}
    alignment _ = 8
{-# LINE 95 "IOCtl.hsc" #-}

    poke p v = do
        (\hsc_ptr -> pokeByteOff hsc_ptr 0) p $ time v
{-# LINE 98 "IOCtl.hsc" #-}
        (\hsc_ptr -> pokeByteOff hsc_ptr 16) p $ typ v
{-# LINE 99 "IOCtl.hsc" #-}
        (\hsc_ptr -> pokeByteOff hsc_ptr 18) p $ code v
{-# LINE 100 "IOCtl.hsc" #-}
        (\hsc_ptr -> pokeByteOff hsc_ptr 20) p $ value v
{-# LINE 101 "IOCtl.hsc" #-}

    peek p = InputEvent <$> (\hsc_ptr -> peekByteOff hsc_ptr 0) p
{-# LINE 103 "IOCtl.hsc" #-}
                        <*> (\hsc_ptr -> peekByteOff hsc_ptr 16) p
{-# LINE 104 "IOCtl.hsc" #-}
                        <*> (\hsc_ptr -> peekByteOff hsc_ptr 18) p
{-# LINE 105 "IOCtl.hsc" #-}
                        <*> (\hsc_ptr -> peekByteOff hsc_ptr 20) p
{-# LINE 106 "IOCtl.hsc" #-}

emit :: Fd -> Int -> Int -> Int -> IO ()
emit fd typ code val = alloca $ \ie_ptr -> do
    let ie = InputEvent emptyTimeVal (fromIntegral typ) (fromIntegral code) (fromIntegral val)
    poke ie_ptr ie
    write fd ie_ptr (sizeOf ie)
