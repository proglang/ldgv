module Networking.Tests where

import Networking.Assert
import Networking.Buffer
import Networking.NetworkBuffer

test :: IO ()
test = testBuffer >> testNetworkBuffer

testBuffer :: IO ()
testBuffer = do
    newBuf <- newBuffer
    writeBufferToList newBuf >>= assertEQ "1: Empty Buffer" []
    tryTakeBuffer newBuf >>= assertEQ "2: Nothing" Nothing
    tryReadBuffer newBuf >>= assertEQ "3: Read Nothing" Nothing
    writeBuffer newBuf 42
    writeBufferToList newBuf >>= assertEQ "4: One Element Buffer" [42]
    cloneBuffer <- cloneBuffer newBuf
    writeBufferToList newBuf >>= assertEQ "5: One Element in Clone" [42]
    writeBuffer cloneBuffer 1337
    writeBufferToList cloneBuffer >>= assertEQ "6: 2 Elements in Clone" [42, 1337]
    writeBufferToList newBuf >>= assertEQ "7: 2 Elements in Buffer" [42, 1337]
    tryReadBuffer cloneBuffer >>= assertEQ "8: Try Read from Clone" (Just 42)
    readBuffer cloneBuffer >>= assertEQ "9: Read from Clone" 42
    takeBuffer cloneBuffer >>= assertEQ "10: Take from Clone " 42
    tryTakeBuffer cloneBuffer >>= assertEQ "11: Try Take from Clone" (Just 1337)
    writeBufferToList newBuf >>= assertEQ "12: 2 Elements in Buffer" [42, 1337]
    writeBufferToList cloneBuffer >>= assertEQ "13: Empty Clone" []
    writeBuffer newBuf 1
    writeBufferToList newBuf >>= assertEQ "14: 3 Elements in Buffer" [42, 1337, 1]
    writeBufferToList cloneBuffer >>= assertEQ "15: 1 Element in Clone" [1]


testNetworkBuffer :: IO ()
testNetworkBuffer = do
    nb <- newNetworkBuffer
    isAllAcknowledged nb >>= assertEQ "1: All acknowledged" True
    write nb 42
    isAllAcknowledged nb >>= assertEQ "2: Not All acknowledged" False
    serializeMinimal nb >>= assertEQ "3: Serial" ([42], 0, 1)
    write nb 1337
    serializeMinimal nb >>= assertEQ "3: Serial" ([42, 1337], 0, 2)
    tryGetAtNB nb 0 >>= assertEQ "4. 42" (Just 42)
    tryGetAtNB nb 1 >>= assertEQ "5. 1337" (Just 1337)
    tryTake nb >>= assertEQ "6. 42" (Just (42, 0))
    getNextOffset nb >>= assertEQ "7. 1" 1
    serializeMinimal nb >>= assertEQ "8: Serial" ([1337], 1, 2)
    updateAcknowledgements nb 1
    isAllAcknowledged nb >>= assertEQ "9. All achnowledged" True


    

    

    