module BufferSpec (spec) where

import Networking.Buffer
import Test.Hspec

spec :: Spec
spec = do
    describe "Buffer IO Test" $ do
        it "Empty list" $ emptyBufferTest `shouldReturn` []
        it "Take from empty" $ tryTakeFromEmptyBufferTest `shouldReturn` Nothing
        it "Read from empty" $ tryReadFromEmptyBufferTest `shouldReturn` Nothing
        it "One Element in Buffer" $ (bufferWith42 >>= writeBufferToList) `shouldReturn` [42]
        it "One Element in Clone" $ (bufferWith42AndClone >>= writeBufferToList . snd) `shouldReturn` [42]
        it "Two Elements in Clone" $ (bufferWith42And1337AndClone >>= writeBufferToList . snd) `shouldReturn` [42, 1337]
        it "Two Elements in Buffer" $ (bufferWith42And1337AndClone >>= writeBufferToList . fst) `shouldReturn` [42, 1337]
        it "Try Read from Clone" $ (bufferWith42And1337AndClone >>= tryReadBuffer . snd) `shouldReturn` Just 42
        it "Read from Clone" $ (bufferWith42And1337AndClone >>= readBuffer . snd) `shouldReturn` 42
        it "Take from Clone" $ (bufferWith42And1337AndClone >>= takeBuffer . snd) `shouldReturn` 42
        it "Try Take from Clone" $ (bufferWith42And1337AndCloneTake42 >>= tryReadBuffer . snd) `shouldReturn` Just 1337
        it "Two Elements in Buffer #2" $ (bufferWith42And1337AndCloneTake42And1337 >>= writeBufferToList . fst) `shouldReturn` [42, 1337]
        it "No Elements in Clone" $ (bufferWith42And1337AndCloneTake42And1337 >>= writeBufferToList . snd) `shouldReturn` []
        it "Three Elements in Buffer " $ (bufferWith42And1337And1AndCloneTake42And1337 >>= writeBufferToList . fst) `shouldReturn` [42, 1337, 1]
        it "One Element in Clone #2" $ (bufferWith42And1337And1AndCloneTake42And1337 >>= writeBufferToList . snd) `shouldReturn` [1]




emptyBufferTest :: IO [Integer]
emptyBufferTest = newBuffer >>= writeBufferToList

tryTakeFromEmptyBufferTest :: IO (Maybe Integer)
tryTakeFromEmptyBufferTest = newBuffer >>= tryTakeBuffer

tryReadFromEmptyBufferTest :: IO (Maybe Integer)
tryReadFromEmptyBufferTest = newBuffer >>= tryReadBuffer


bufferWith42 :: IO (Buffer Integer)
bufferWith42 = do
    newBuf <- newBuffer
    writeBuffer newBuf 42
    return newBuf

bufferWith42AndClone :: IO (Buffer Integer, Buffer Integer)
bufferWith42AndClone = do
    buf <- bufferWith42
    clone <- cloneBuffer buf
    return (buf, clone)

bufferWith42And1337AndClone :: IO (Buffer Integer, Buffer Integer)
bufferWith42And1337AndClone = do
    (buf, clone) <- bufferWith42AndClone
    writeBuffer clone 1337
    return (buf, clone)

bufferWith42And1337AndCloneTake42 :: IO (Buffer Integer, Buffer Integer)
bufferWith42And1337AndCloneTake42 = do
    (buf, clone) <-bufferWith42And1337AndClone
    takeBuffer clone
    return (buf, clone)

bufferWith42And1337AndCloneTake42And1337 :: IO (Buffer Integer, Buffer Integer)
bufferWith42And1337AndCloneTake42And1337 = do
    (buf, clone) <-bufferWith42And1337AndCloneTake42
    tryTakeBuffer clone
    return (buf, clone)

bufferWith42And1337And1AndCloneTake42And1337 :: IO (Buffer Integer, Buffer Integer)
bufferWith42And1337And1AndCloneTake42And1337 = do
    (buf, clone) <-bufferWith42And1337AndCloneTake42And1337
    writeBuffer buf 1
    return (buf, clone)