{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}


-- General
import BasePrelude hiding (second)
-- Testing
import Test.Hspec
-- Time
import Data.Time.Clock.TAI
import Data.Time.Zones
-- Tom-specific
import Tom.When
import Tom.Utils


main :: IO ()
main = do
  moscowTZ <- loadSystemTZ "Europe/Moscow"
  let pData = WhenParserData {
        -- i.e. 14:09:44 in Moscow
        _currentTime = read "2013-02-01 10:09:44.900000 UTC",
        _localTZ     = moscowTZ }
  let absoluteTime = utcToAbsoluteTime (_currentTime pData)
  hspec $ do
    context "parsers" $ do
      context "moment" $ do
        specify "2pm (will be next day)" $ do
          x <- parseWhen' pData "2pm"
          x `shouldBe` Right Mask {
            year     = Just 2013,
            month    = Just 2,
            day      = Just 2,
            hour     = Just 14,
            minute   = Just 0,
            second   = Just 0,
            weekdays = Nothing,
            timezone = Nothing }
        specify "3pm (will be today)" $ do
          x <- parseWhen' pData "3pm"
          x `shouldBe` Right Mask {
            year     = Just 2013,
            month    = Just 2,
            day      = Just 1,
            hour     = Just 15,
            minute   = Just 0,
            second   = Just 0,
            weekdays = Nothing,
            timezone = Nothing }
        specify "3.10pm" $ do
          x <- parseWhen' pData "3.10pm"
          x `shouldBe` Right Mask {
            year     = Just 2013,
            month    = Just 2,
            day      = Just 1,
            hour     = Just 15,
            minute   = Just 10,
            second   = Just 0,
            weekdays = Nothing,
            timezone = Nothing }
        specify ".05 (will happen next hour)" $ do
          x <- parseWhen' pData ".05"
          x `shouldBe` Right Mask {
            year     = Just 2013,
            month    = Just 2,
            day      = Just 1,
            hour     = Just 15,
            minute   = Just 5,
            second   = Just 0,
            weekdays = Nothing,
            timezone = Nothing }
        specify "29th day of the month (will happen next month)" $ do
          x <- parseWhen' pData "29/4pm"
          x `shouldBe` Right Mask {
            year     = Just 2013,
            month    = Just 3,
            day      = Just 29,
            hour     = Just 16,
            minute   = Just 0,
            second   = Just 0,
            weekdays = Nothing,
            timezone = Nothing }
        specify "February 29 (will be in 3 years)" $ do
          x <- parseWhen' pData "2-29/4pm"
          x `shouldBe` Right Mask {
            year     = Just 2016,
            month    = Just 2,
            day      = Just 29,
            hour     = Just 16,
            minute   = Just 0,
            second   = Just 0,
            weekdays = Nothing,
            timezone = Nothing }
      context "duration" $ do
        specify "1h2m3s" $ do
          x <- parseWhen' pData "1h2m3s"
          x `shouldBe` Right Moment {
            moment = addAbsoluteTime (3600+2*60+3) absoluteTime }
      context "periodic" $ do
        specify "every 1h2m3s" $ do
          x <- parseWhen' pData "every 1h2m3s"
          x `shouldBe` Right Periodic {
            period = 3600+2*60+3,
            start  = addAbsoluteTime (3600+2*60+3) absoluteTime }

{-

Date format:

  * Y-M-D
  * M-D
  * D

Time format: [H][.MM][:SS][am|pm][,timezone] (all components can be omitted).

If the resulting time is always in the past, the function will fail.

A timezone can be specified as an abbreviation. At the moment, only a handful
of abbreviations are supported.

-}
