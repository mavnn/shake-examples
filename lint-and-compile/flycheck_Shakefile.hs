module Shakefile
  ( main
  )
where

import Development.Shake
import Development.Shake.FilePath

main :: IO ()
main =
  shakeArgs
    shakeOptions
      { shakeFiles = "_build"
      , shakeChange = ChangeModtimeAndDigest
      , shakeColor = True
      , shakeThreads = 4 -- default to multicore!
      } $ do
    want
      [ "_build" </> "main" <.> exe
      , -- Make sure that even our Shakefile is tidy
      "Shakefile.hs"
      ]
    rules

rules :: Rules ()
rules = do
  -- Clean build artifacts (including shake history)
  phony "clean" $ do
    putNormal "Cleaning _build"
    removeFilesAfter "_build" ["//*"]
  -- Build our Haskell application
  "_build" </> "main" <.> exe %> \out -> do
    src <- getDirectoryFiles "" ["src//*.hs"]
    need src
    cmd_
      "ghc"
      ("src" </> "main.hs")
      "-isrc"
      "-outputdir"
      "_build"
      "-o"
      out
  -- Format and lint our source files
  batch 10 ("//*.hs" %>)
    ( \out -> do
      historyDisable
      cmd_ "ormolu" "-m" "inplace"
      pure out
    )
    (cmd_ "hlint")
