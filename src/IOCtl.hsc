{-# LANGUAGE CPP, ForeignFunctionInterface #-}
module IOCtl where

import Data.Int
import Data.Monoid
import Data.Word
import Foreign
import Foreign.C
import Foreign.Ptr
import System.Posix.Types

#include "linux/uinput.h"

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
    sizeOf    _ = #size struct input_id
    alignment _ = #alignment struct input_id

    poke p v = do
        #{poke struct input_id, bustype} p $ bustype v
        #{poke struct input_id, vendor} p  $ vendorId v
        #{poke struct input_id, product} p $ productId v
        #{poke struct input_id, version} p $ version v

    peek p = InputId <$> #{peek struct input_id, bustype} p
                     <*> #{peek struct input_id, vendor}  p
                     <*> #{peek struct input_id, product} p
                     <*> #{peek struct input_id, version} p

instance Storable UInputSetup where
    sizeOf    _ = #{size struct uinput_setup}
    alignment _ = #{alignment struct uinput_setup}

    poke p v = do
        #{poke struct uinput_setup, id} p $ input_id v
        #{poke struct uinput_setup, name} p $ name v
        #{poke struct uinput_setup, ff_effects_max} p $ ff_effects_max v

    peek p = pure UInputSetup
                 <*> #{peek struct uinput_setup, id} p
                 <*> #{peek struct uinput_setup, name} p
                 <*> #{peek struct uinput_setup, ff_effects_max} p

instance Storable TimeVal where
    sizeOf    _ = #size struct timeval
    alignment _ = #alignment struct timeval

    poke p v = do
        #{poke struct timeval, tv_sec} p $ tv_sec v
        #{poke struct timeval, tv_usec} p $ tv_usec v

    peek p = TimeVal <$> #{peek struct timeval, tv_sec} p
                     <*> #{peek struct timeval, tv_usec} p

instance Storable InputEvent where
    sizeOf    _ = #size struct input_event
    alignment _ = #alignment struct input_event

    poke p v = do
        #{poke struct input_event, time} p $ time v
        #{poke struct input_event, type} p $ typ v
        #{poke struct input_event, code} p $ code v
        #{poke struct input_event, value} p $ value v

    peek p = InputEvent <$> #{peek struct input_event, time} p
                        <*> #{peek struct input_event, type} p
                        <*> #{peek struct input_event, code} p
                        <*> #{peek struct input_event, value} p

emit :: Fd -> Int -> Int -> Int -> IO ()
emit fd typ code val = alloca $ \ie_ptr -> do
    let ie = InputEvent emptyTimeVal (fromIntegral typ) (fromIntegral code) (fromIntegral val)
    poke ie_ptr ie
    write fd ie_ptr (sizeOf ie)
