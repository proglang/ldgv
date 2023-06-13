{-# LANGUAGE LambdaCase #-}

module Networking.Assert where

import Control.Exception


newtype AssertException = TestErrorAssertEQ String

instance Show AssertException where
    show = \case 
        TestErrorAssertEQ err -> "AssertException (TestErrorAssertEQ): " ++ err


instance Exception AssertException


assertEQ :: Eq a => String -> a -> a -> IO ()
assertEQ err v1 v2 = if v1==v2 then putStrLn $ "Assertion Success: "++ err else throw $ TestErrorAssertEQ err