{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Shakefiles.Generator
  ( generator
  , getGeneratedFiles
  , runIdOracle
  )
where

import Control.Applicative ()
import Data.Aeson
import qualified Data.ByteString.Lazy as B
import qualified Data.UUID as UUID
import Data.UUID.V4 (nextRandom)
import Development.Shake
import Development.Shake.Classes
import qualified System.Directory as Directory

newtype GetRunId
  = GetRunId (FilePath, [FilePattern])
  deriving (Show, Typeable, Eq, Hashable, Binary, NFData)

type instance RuleResult GetRunId = UUID.UUID

runIdOracle :: GetRunId -> Action UUID.UUID
runIdOracle (GetRunId (filePath, patterns)) = do
  recordExists <- liftIO $ Directory.doesFileExist filePath
  if recordExists
  then
    do
      recorded <- decode <$> liftIO (B.readFile filePath)
      case recorded of
        Just (lastRunId, generatedFiles) -> do
          filesOnDisk <- liftIO $ getDirectoryFilesIO "" patterns
          if filesOnDisk /= generatedFiles
          then liftIO nextRandom
          else pure lastRunId
        Nothing ->
          liftIO nextRandom
  else liftIO nextRandom

recordGeneratedFiles :: UUID.UUID -> FilePath -> [FilePattern] -> Action ()
recordGeneratedFiles runId out patterns = do
  filesCreated <- liftIO $ getDirectoryFilesIO "" patterns
  liftIO $ B.writeFile out $ encode (runId, filesCreated)

generator :: FilePath -> [FilePattern] -> Action () -> Rules ()
generator out generatedPatterns generationCmd = do
  generatedPatterns |%> \_ -> need [out]
  out %> \_ -> do
    runId <- askOracle $ GetRunId (out, generatedPatterns)
    liftIO $ removeFiles "" generatedPatterns
    generationCmd
    recordGeneratedFiles runId out generatedPatterns

getGeneratedFiles :: FilePath -> Action [FilePath]
getGeneratedFiles out = do
  need [out]
  recordFile <- decode <$> liftIO (B.readFile out)
  case recordFile of
    Just (_ :: UUID.UUID, rf) ->
      pure rf
    Nothing ->
      fail ""
