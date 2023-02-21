{-# LANGUAGE CPP #-}

module Networking.Buffer where

{-
Buffer reuses and adapts vast amounts of code from the Control.Concurrent.Chan implementation licenced under:

-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Concurrent.Chan
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  stable
-- Portability :  non-portable (concurrency)
--
-- Unbounded channels.
--
-- The channels are implemented with @MVar@s and therefore inherit all the
-- caveats that apply to @MVar@s (possibility of races, deadlocks etc). The
-- stm (software transactional memory) library has a more robust implementation
-- of channels called @TChan@s.
--
-----------------------------------------------------------------------------

This library (libraries/base) is derived from code from several
sources: 

  * Code from the GHC project which is largely (c) The University of
    Glasgow, and distributable under a BSD-style license (see below),

  * Code from the Haskell 98 Report which is (c) Simon Peyton Jones
    and freely redistributable (but see the full license for
    restrictions).

  * Code from the Haskell Foreign Function Interface specification,
    which is (c) Manuel M. T. Chakravarty and freely redistributable
    (but see the full license for restrictions).

The full text of these licenses is reproduced below.  All of the
licenses are BSD-style or compatible.

-----------------------------------------------------------------------------

The Glasgow Haskell Compiler License

Copyright 2004, The University Court of the University of Glasgow. 
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

- Redistributions of source code must retain the above copyright notice,
this list of conditions and the following disclaimer.
 
- Redistributions in binary form must reproduce the above copyright notice,
this list of conditions and the following disclaimer in the documentation
and/or other materials provided with the distribution.
 
- Neither name of the University nor the names of its contributors may be
used to endorse or promote products derived from this software without
specific prior written permission. 

THIS SOFTWARE IS PROVIDED BY THE UNIVERSITY COURT OF THE UNIVERSITY OF
GLASGOW AND THE CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES,
INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND
FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
UNIVERSITY COURT OF THE UNIVERSITY OF GLASGOW OR THE CONTRIBUTORS BE LIABLE
FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH
DAMAGE.

-----------------------------------------------------------------------------

Code derived from the document "Report on the Programming Language
Haskell 98", is distributed under the following license:

  Copyright (c) 2002 Simon Peyton Jones

  The authors intend this Report to belong to the entire Haskell
  community, and so we grant permission to copy and distribute it for
  any purpose, provided that it is reproduced in its entirety,
  including this Notice.  Modified versions of this Report may also be
  copied and distributed for any purpose, provided that the modified
  version is clearly presented as such, and that it does not claim to
  be a definition of the Haskell 98 Language.

-----------------------------------------------------------------------------

Code derived from the document "The Haskell 98 Foreign Function
Interface, An Addendum to the Haskell 98 Report" is distributed under
the following license:

  Copyright (c) 2002 Manuel M. T. Chakravarty

  The authors intend this Report to belong to the entire Haskell
  community, and so we grant permission to copy and distribute it for
  any purpose, provided that it is reproduced in its entirety,
  including this Notice.  Modified versions of this Report may also be
  copied and distributed for any purpose, provided that the modified
  version is clearly presented as such, and that it does not claim to
  be a definition of the Haskell 98 Foreign Function Interface.

-----------------------------------------------------------------------------
-}

import Control.Concurrent.MVar
import Control.Exception
import Control.Monad



#define _UPK_(x) {-# UNPACK #-} !(x)

data Buffer a = Buffer {bufferReadHead :: _UPK_(MVar (Chain a)), bufferSharedWriteHead :: _UPK_(MVar (Chain a))}
    deriving Eq

type Chain a = MVar (Element a)

data Element a = Element {elementValue :: a, nextElement :: _UPK_(Chain a)}
    deriving Eq

newBuffer :: IO (Buffer a)
newBuffer = do
    element <- newEmptyMVar
    readHead <- newMVar element
    sharedWriteHead <- newMVar element
    return $ Buffer readHead sharedWriteHead

writeBuffer :: Buffer a -> a -> IO ()
writeBuffer buffer@(Buffer readVar sharedWriteVar) value = do
    newElement <- newEmptyMVar
    mask_ $ do
        oldElement <- takeMVar sharedWriteVar
        putMVar oldElement $ Element value newElement
        putMVar sharedWriteVar newElement

takeBuffer :: Buffer a -> IO a
takeBuffer buffer@(Buffer readVar sharedWriteVar) = do
    readHead <- takeMVar readVar
    (Element value bufferStart) <- readMVar readHead
    putMVar readVar bufferStart
    return value

tryTakeBuffer :: Buffer a -> IO (Maybe a)
tryTakeBuffer buffer@(Buffer readVar sharedWriteVar) = do
    mbyReadHead <- tryTakeMVar readVar
    case mbyReadHead of
        Just readHead -> do
            mbyElement <- tryReadMVar readHead
            case mbyElement of
                Just (Element value bufferStart) -> do
                    putMVar readVar bufferStart
                    return $ Just value
                Nothing -> do 
                    putMVar readVar readHead
                    return Nothing
        Nothing -> return Nothing

readBuffer :: Buffer a -> IO a
readBuffer bufer@(Buffer readVar sharedWriteVar) = do
    readHead <- readMVar readVar
    (Element value bufferStart) <- readMVar readHead
    return value

tryReadBuffer :: Buffer a -> IO (Maybe a)
tryReadBuffer buffer@(Buffer readVar sharedWriteVar) = do
    mbyReadHead <- tryReadMVar readVar
    case mbyReadHead of
        Just readHead -> do
            mbyElement <- tryReadMVar readHead
            case mbyElement of
                Just (Element value bufferStart) -> do
                    return $ Just value
                Nothing -> return Nothing
        Nothing -> return Nothing

-- Similar to dupChan in the Base package
duplicateBuffer :: Buffer a -> IO (Buffer a)
duplicateBuffer buffer@(Buffer readVar sharedWriteVar) = do
    element <- readMVar sharedWriteVar
    newReadVar <- newMVar element
    return $ Buffer newReadVar sharedWriteVar

-- Duplicate Buffer along with contents
cloneBuffer :: Buffer a -> IO (Buffer a)
cloneBuffer buffer@(Buffer readVar sharedWriteVar) = do
    element <- readMVar readVar
    newReadVar <- newMVar element
    return $ Buffer newReadVar sharedWriteVar

consumeBufferToList :: Buffer a -> IO [a]
consumeBufferToList buffer = do
    mbyFirstElement <- tryTakeBuffer buffer
    case mbyFirstElement of
        Just firstElement -> do
            listTail <- consumeBufferToList buffer
            return $ firstElement:listTail
        Nothing -> return []

writeBufferToList :: Buffer a -> IO [a]
writeBufferToList buffer = do
    clone <- cloneBuffer buffer
    consumeBufferToList clone

getAt :: Buffer a -> Int -> IO a
getAt buffer count = do
    clone <- cloneBuffer buffer
    -- Take count-1 times
    forM_ [1..count] $ \_ -> takeBuffer clone
    takeBuffer clone

tryGetAt :: Buffer a -> Int -> IO (Maybe a)
tryGetAt buffer count = do
    clone <- cloneBuffer buffer
    tryGetAtInternal clone count
    where
        tryGetAtInternal :: Buffer a -> Int -> IO (Maybe a)
        tryGetAtInternal buffer count | count <= 0 = tryTakeBuffer buffer
                                      | otherwise = do
            mbyVal <- tryTakeBuffer buffer
            case mbyVal of
                Just val -> tryGetAtInternal buffer $ count-1
                Nothing -> return Nothing