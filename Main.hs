{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Applicative
import Control.Monad
--import Control.Monad.IO.Class
import Data.List
import Data.Maybe
import qualified Data.Text as T
import Shelly (Sh)
import qualified Shelly as S

getGithubPath :: Sh (Maybe String)
getGithubPath = do
  r <- T.lines <$> S.silently (S.run "git" ["remote", "-v"])
  let x = filter (liftM2 (&&) ("(push)" `T.isInfixOf`) ("github.com" `T.isInfixOf`)) r
      y = init . drop 1 . dropWhile (/= ':') . dropWhileEnd (/= '.') . T.unpack <$> listToMaybe x
    in return y

main :: IO ()
main = do
  repo <- S.shelly getGithubPath
  putStrLn (fromMaybe "This does not appear to be a Github repo" repo)
