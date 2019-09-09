module Shakefiles.HaskellGenerator
  ( createHaskellFiles
  )
where

import Data.Char
import qualified Data.HashMap.Strict as HM
import Data.List (intercalate)
import Data.Maybe
import Data.Yaml
import Development.Shake
import Development.Shake.FilePath

createHaskellFiles :: [FilePath] -> Action ()
createHaskellFiles yamlFiles = do
  _ <- parallel $ fmap createHaskellFile yamlFiles
  pure ()

createHaskellFile :: FilePath -> Action ()
createHaskellFile yaml = do
  yamlObj <- liftIO $ decodeFileThrow yaml
  let target = "src" </> "Generated" </> takeBaseName yaml <.> ".hs"
  writeFileLines target $ haskellLines yamlObj
  cmd_ "ormolu" "-m" "inplace" target

haskellLines :: HM.HashMap String String -> [String]
haskellLines yamlObj =
  [ "module Generated." ++ name ++ " where"
  , "data " ++ titleCase name ++ " = " ++ titleCase name ++ " {"
  , intercalate ", "
    ( fmap (\kv -> "    " ++ fst kv ++ " :: " ++ snd kv)
      (filter (\kv -> fst kv /= "name") $ HM.toList yamlObj)
    )
  , "  } deriving (Show)"
  ]
  where
    name =
      fromMaybe
        (error "Yaml source must specify a name")
        (HM.lookup "name" yamlObj)

titleCase :: String -> String
titleCase [] = []
titleCase (x : xs) = toUpper x : xs
