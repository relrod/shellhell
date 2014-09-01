{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Applicative
import Control.Monad
import Data.List
import Data.Maybe
import Data.Monoid
import qualified Data.Text as T
import Shelly (Sh)
import qualified Shelly as S
import System.Environment (getArgs)
import System.FilePath

getGithubPath :: Sh (Maybe String)
getGithubPath = do
  r <- T.lines <$> S.silently (S.run "git" ["remote", "-v"])
  let x = filter (liftM2 (&&) ("(push)" `T.isInfixOf`) ("github.com" `T.isInfixOf`)) r
      y = init . drop 1 . dropWhile (/= ':') . dropWhileEnd (/= '.') . T.unpack <$> listToMaybe x
    in return y

scriptPreamble :: Sh (Maybe [String])
scriptPreamble = do
  path <- getGithubPath
  case path of
    Nothing -> return Nothing
    Just p  ->
      let preamble = [ "# Generated via docs-deploy:"
                     , "# https://github.com/CodeBlock/docs-deploy"
                     , "git_project=\"" <> p <> "\""
                     , "git_url=\"" <> "git@github.com:" <> p <> ".git\""
                     , "cwd=\"$( cd \"${BASH_SOURCE[0]%/*}\" && pwd )\""
                     , "cd \"$cwd/..\""
                     ]
        in return (Just preamble)

scriptDeploy :: String -> [String]
scriptDeploy docsPath =
  [ "function deploy {"
  , "  rand_dir=`mktemp -d`"
  , "  git clone \"$git_url\" \"$rand_dir/repo\""
  , "  pushd \"$rand_dir/repo\""
  , "  git checkout gh-pages"
  , "  git rm -rf docs"
  , "  popd"
  , "  cp -rv " <> docsPath <> " \"$rand_dir/repo/docs\""
  , "  pushd \"$rand_dir/repo\""
  , "  git add -A"
  , "  git commit -m '[scripted] documentation deployment'"
  , "  git push origin gh-pages"
  , "  popd"
  , "  rm -rf \"$rand_dir\""
  , "}"
  ]

main :: IO ()
main = do
  args <- getArgs
  when (length args < 2) (error "No project template and/or docs path supplied")
  let template = head args
  script <- S.shelly scriptPreamble
  putStrLn (maybe (error "This does not appear to be a Github repo") (intercalate "\n") script)
  readFile ("environments" </> template <.> "sh") >>= putStrLn
  putStrLn (intercalate "\n" (scriptDeploy (head (tail args))))
  putStrLn "generate"
  putStrLn "deploy"
