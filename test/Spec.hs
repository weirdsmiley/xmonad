module Main where

import Control.Monad
import qualified Data.Map as M
import Preferences
import Workspaces (myWorkspaces)
import XMonad
import Test.Hspec
import Test.Hspec.QuickCheck
import Control.Exception (evaluate)


main :: IO ()
main = hspec $ do
    -- Workspaces tests
    describe "Workspaces.myWorkspaces" $ do
        it "contains 10 workspaces" $
            length myWorkspaces `shouldBe` 10
        -- it "contains workspace ids from 1 to 10" $
        --     myWorkspaces `shouldBe` ["1", "2", "3", "4", "5", "6", "7", "8", "9", "10"]

    -- Preferences tests
    describe "Preferences::" $ do
        it "myTerminal is kitty" $
            myTerminal `shouldBe` "kitty"
        it "myGaps is 5" $
            myGaps `shouldBe` 5
        it "myBorderWidth is 0" $
            myBorderWidth `shouldBe` 0
        it "myModKey is mod1Mask" $
            myModKey `shouldBe` mod1Mask
        it "myFocusFollowsMouse is True" $
            myFocusFollowsMouse `shouldBe` True
        it "myClickJustFocuses is False" $
            myClickJustFocuses `shouldBe` False
        it "myBrowser is firefox" $
            myBrowser `shouldBe` "firefox"
        it "myLockscreen contains xsecurelock" $
            "xsecurelock" `elem` myLockscreen
