module Shakefile
  ( main
  )
where

import Development.Shake
import Development.Shake.FilePath
import Development.Shake.Util
import Shakefiles.Generator (generator, runIdOracle)
import Shakefiles.HaskellGenerator (createHaskellFiles)

main :: IO ()
main =
  shakeArgs
    shakeOptions
      { shakeFiles = "_build"
      , shakeChange = ChangeModtimeAndDigest
      , shakeColor = True
      , shakeThreads = 4 -- default to multicore!
      , shakeShare = Just "_build/share"
      } $ do
    want
      [ "_build" </> "main" <.> exe
      , -- Make sure that even our Shakefile is tidy
      "format-shake"
      ]
    rules

rules :: Rules ()
rules = do
  -- Clean build artifacts (including shake history)
  phony "clean" $ do
    putNormal "Cleaning _build"
    removeFilesAfter "_build" ["//*"]
  -- Make the shakefiles pretty
  phony "format-shake" $ do
    shakeModules <- getDirectoryFiles "" ["Shakefiles//*.hs"]
    need $ "Shakefile.hs" : shakeModules
  -- Build our Haskell application
  "_build" </> "main" <.> exe %> \out -> do
    need ["_build/haskell_generation.log"]
    cmd_
      "ghc"
      ("src" </> "main.hs")
      "-isrc"
      "-dep-suffix hs"
      "-outputdir"
      "_build"
      "-o"
      out
    withTempFile
      ( \tmpFile -> do
        cmd_
          Shell
          "ghc"
          ("src" </> "main.hs")
          "-isrc"
          "-outputdir"
          "_build"
          "-o"
          out
          "-dep-makefile"
          tmpFile
          "-dep-suffix ''"
          "-M"
        makeStuff <- liftIO $ readFile tmpFile
        putNormal makeStuff
        neededMakefileDependencies tmpFile
      )
  -- Format and lint our source files
  "//*.hs" %> \out -> do
    historyDisable
    cmd_ "hlint --refactor --refactor-options=-i" out
    cmd_ "ormolu" "-m" "inplace" out
  -- Target ensures all haskell files are built
  _ <- addOracle runIdOracle
  priority 2 $
    generator
      "_build/haskell_generation.log"
      ["src" </> "Generated//*.hs"]
      writeHaskellFiles
  where
    writeHaskellFiles = do
      yamlFiles <- getDirectoryFiles "" ["yaml_types//*.yaml"]
      need yamlFiles
      createHaskellFiles yamlFiles
