module NetworkBufferSpec (spec) where

import Networking.Buffer
import Networking.NetworkBuffer
import Test.Hspec

spec :: Spec
spec = do
    describe "NetworkBuffer IO Test" $ do
        it "All Acknowledged" $ (nb >>=  isAllAcknowledged) `shouldReturn` True
        it "Not All Acknowledged " $ (nb42 >>= isAllAcknowledged) `shouldReturn` False
        it "Serialize" $ (nb42 >>= serializeMinimal) `shouldReturn` ([42], 0, 1)
        it "Serialize #2" $ (nb42And1337 >>= serializeMinimal) `shouldReturn` ([42, 1337], 0, 2)
        it "Try Read at 0" $ (nb42And1337 >>= \x -> tryGetAtNB x 0) `shouldReturn` Just 42
        it "Try Read at 1" $ (nb42And1337 >>= \x -> tryGetAtNB x 1) `shouldReturn` Just 1337
        it "Try Take" $ (nb42And1337 >>= tryTake) `shouldReturn` Just (42, 0)
        it "Next Offset" $ (nb1337 >>= getNextOffset) `shouldReturn` 1
        it "Serialize #3" $ (nb1337 >>= serializeMinimal) `shouldReturn` ([1337], 1, 2)
        it "All Acknowledged #2" $ (nb1337AllAck >>= isAllAcknowledged) `shouldReturn` True
        it "Serialize #4" $ (nb1337AllAck >>= serializeMinimal) `shouldReturn` ([], 2, 2)

nb :: IO (NetworkBuffer Integer)
nb = newNetworkBuffer

nb42 :: IO (NetworkBuffer Integer) 
nb42 = do
    nb <- newNetworkBuffer
    write nb 42
    return nb

nb42And1337 :: IO (NetworkBuffer Integer)
nb42And1337 = do
    nb <- nb42
    write nb 1337
    return nb

nb1337 :: IO (NetworkBuffer Integer)
nb1337 = do
    nb <- nb42And1337
    tryTake nb
    return nb

nb1337AllAck :: IO (NetworkBuffer Integer)
nb1337AllAck = do
    nb <- nb1337
    updateAcknowledgements nb 1
    return nb
