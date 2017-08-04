{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
module Main where

import Control.Concurrent
import Data.Int
import Foreign
import Foreign.C
import Foreign.Marshal.Alloc
import System.Posix.Types
import System.Posix.IO

import Events
import IOCtl

inputId :: InputId
inputId = InputId { bustype = bus_usb
                  , vendorId = 0x1234
                  , productId = 0x5678
                  , version = 0x0000
                  }

uInputSetup :: CString -> UInputSetup
uInputSetup name = UInputSetup { input_id = inputId
                               , name = name
                               , ff_effects_max = 0x0000
                               }

main :: IO ()
main = do
    fd <- openFd "/dev/uinput" WriteOnly Nothing (defaultFileFlags{nonBlock=True})

    ioctl_i fd ui_set_evbit ev_key
    ioctl_i fd ui_set_keybit key_f

    alloca $ \ptr -> withCString "Test device" $ \name -> do
        poke ptr (uInputSetup name)
        ioctl fd ui_dev_setup ptr

    ioctl fd ui_dev_create nullPtr
    threadDelay 1000000

    emit fd ev_key key_f 1
    emit fd ev_syn syn_report 0
    emit fd ev_key key_f 0
    emit fd ev_syn syn_report 0

    threadDelay 1000000
    ioctl fd ui_dev_destroy nullPtr

    closeFd fd
