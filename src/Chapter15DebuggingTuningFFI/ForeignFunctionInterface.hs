{-# LANGUAGE ForeignFunctionInterface #-}
  
module Chapter15DebuggingTuningFFI.ForeignFunctionInterface where

-- https://hackage.haskell.org/package/base-4.9.1.0/docs/Foreign-C.html
-- https://hackage.haskell.org/package/base-4.9.1.0/docs/Foreign-C-String.html
import Foreign.C.String
import Foreign.C.Types
  
foreign import ccall "trim" trim :: CString -> CInt

-- https://wiki.haskell.org/Foreign_Function_Interface
-- http://chimera.labs.oreilly.com/books/1230000000929/ch15.html#sec_conc-ffi
-- https://wiki.haskell.org/FFI_Introduction


main = do
  putStrLn "Foreign function interface example"
  foo <- newCString "   foo    "
  let fooLengthTrimmed = trim foo
  -- fooT <- peekCString foo'
  putStrLn $ show fooLengthTrimmed


