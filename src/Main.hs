{-# LANGUAGE OverloadedStrings #-}

module Main where

import           BasePrelude hiding (some)
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import           Shelly
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

---

newtype Stylus = Stylus Word
newtype Pad    = Pad Word

infoP :: Parsec Void T.Text (Maybe (Stylus, Pad))
infoP = f . M.fromList <$> some line
  where f m = curry (Stylus *** Pad) <$> M.lookup "STYLUS" m <*> M.lookup "PAD" m

line :: Parsec Void T.Text (T.Text, Word)
line = L.lexeme space $ do
  skipManyTill anyChar $ string "id:"
  space
  w <- L.decimal
  space
  string "type:"
  space
  t <- T.strip <$> takeWhileP Nothing (/= '\n')
  pure (t, w)

-- | By default, the tablet matches itself to the entire combined monitor area.
-- This is useless for art, so the command below clamps it to only my second,
-- large monitor.
useBigMonitorOnly :: Stylus -> Sh ()
useBigMonitorOnly (Stylus s) = run_ "xsetwacom" ["--set", T.pack $ show s, "MapToOutput", "HDMI2"]

-- | Matches the usual 16:10 drawing area on the tablet to the 16:9 ratio of
-- the monitor. About 1cm of drawing space is lost at the bottom of the tablet,
-- but at least strokes are no longer distorted horizontally.
fixAspectRatio :: Stylus -> Sh ()
fixAspectRatio (Stylus s) = run_ "xsetwacom" ["--set", T.pack $ show s, "Area", "0", "0", "15200", "8550"]

-- | `p` is the "pen" shortcut in Piskel.
penButton :: Pad -> Sh ()
penButton (Pad p) = run_ "xsetwacom" ["--set", T.pack $ show p, "Button", "1", "key p"]

-- | `e` is the "eraser" shortcut in Piskel.
eraserButton :: Pad -> Sh ()
eraserButton (Pad p) = run_ "xsetwacom" ["--set", T.pack $ show p, "Button", "2", "key e"]

-- | While the first three pad buttons have Id's 1, 2, and 3, the right-most button
-- has Id 8.
undoButton :: Pad -> Sh ()
undoButton (Pad p) = run_ "xsetwacom" ["--set", T.pack $ show p, "Button", "8", "key +ctrl z -ctrl"]

-- | `xsetwacom --list devices` returns nothing when the tablet isn't connected.
tabletInfo :: Sh T.Text
tabletInfo = run "xsetwacom" ["--list", "devices"]

main :: IO ()
main = shelly . print_stdout False $ do
  info <- tabletInfo
  case parse infoP "Wacom Info" info of
    Left err -> echo . T.pack $ parseErrorPretty err
    Right Nothing -> echo "Couldn't parse the device Ids."
    Right (Just (s, p)) -> do
      useBigMonitorOnly s
      fixAspectRatio s
      penButton p
      eraserButton p
      undoButton p
      echo "Tablet configured!"
