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
      , "Shakefile.hs"
      ]
    rules

rules :: Rules ()
rules = do
  phony "clean" $ do
    putNormal "Cleaning _build"
    removeFilesAfter "_build" ["//*"]
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
  batch 10 ("//*.hs" %>)
    ( \out -> do
      cmd_ "ormolu" "-m" "inplace" out
      pure out
    )
    (cmd_ "hlint")
