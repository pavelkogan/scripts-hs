{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main (main) where

import qualified UnliftIO.Directory as Directory

import ClassyPrelude
import Control.Monad.Catch
import Data.Text.Conversions
import Options.Applicative
import Path
import Path.IO
import System.Exit
import System.FilePath (isValid)
import System.Process.Typed
import Zenity

newtype Symlink = Symlink { getSymlink :: Path Abs File } deriving (Show, Eq)

fromSymlink :: Symlink -> FilePath
fromSymlink = fromAbsFile . getSymlink

toSymlink :: MonadIO m => Path Abs File -> m Symlink
toSymlink p = pure (Symlink p)
  -- can't check because tmsu FUSE dies when checking the symlink:
  -- unlessM (isSymlink p) $ throwString ("not a symlink: " <> toFilePath p)

type Tag = Text

parserInfo :: ParserInfo [FilePath]
parserInfo = info (helper <*> parser) (header "tmsu-add-tags")
  where
    parser = some $ pathArgument (metavar "FILES" <> help "files to tag")
    pathArgument = argument $
      maybeReader (\s -> if isValid s then Just s else Nothing)

main :: IO ()
main = handleAny (zen Error . tshow) $ do
  args <- execParser parserInfo
  argPaths <- traverse toAbsolute args
  shouldFollowLinks <- not <$> inTaggedTree (headEx argPaths)
  files <- if shouldFollowLinks
    then followLinks =<< traverse toSymlink argPaths
    else return argPaths
  tags <- getTags
  addTags tags files

toAbsolute :: FilePath -> IO (Path Abs File)
toAbsolute = parseAbsFile <=< Directory.makeAbsolute

followLinks :: MonadIO m => [Symlink] -> m [Path Abs File]
followLinks links = do
  paths <- traverse readlink links
  filterM doesFileExist paths

readlink :: MonadIO m => Symlink -> m (Path Abs File)
readlink lnk = do
  ret <- readProcessStdout_ $ proc "readlink" ["-q", fromSymlink lnk]
  let mpath = parseAbsFile =<< map fromText (listToMaybe (asTextLines ret))
  case mpath of
    Nothing -> throwString $ "readlink failed for: " <> fromSymlink lnk
    Just path -> return path
  where
    asTextLines :: DecodeText Maybe (UTF8 a) => a -> [Text]
    asTextLines b = join . maybeToList . map lines $ decodeText (UTF8 b)

getTags :: IO [Tag]
getTags = do
  let dialog = Entry def{ text = Just "Enter tags to add:" }
  mentry <- zenity def{ title = Just "Add tags" } dialog
  let entry = maybeToList mentry
  return $ words =<< entry

addTags :: (MonadUnliftIO m, MonadMask m) => [Tag] -> [Path Abs File] -> m ()
addTags tags files = withCurrentDir (parent (headEx files)) $ do
  let tagStr = fromText $ unwords tags
  let args = ["tag", "--tags=\""<>tagStr<>"\""] ++ (toFilePath <$> files)
  (ec, sout, serr) <- readProcess $ proc "tmsu" args
  case ec of
    ExitSuccess -> unless (sout == mempty) $ zen Notification $ decodeUtf8 $ toStrict sout
    ExitFailure _ -> zen Warning $ decodeUtf8 $ toStrict serr

inTaggedTree :: (MonadUnliftIO m, MonadMask m) => Path Abs File -> m Bool
inTaggedTree file = withCurrentDir (parent file) $ do
  ec <- runProcess $ proc "tmsu" ["info"]
  return $ ec == ExitSuccess

type DialogType = InfoFlags -> Dialog () -- Error | Warning | Notification
zen :: MonadIO m => DialogType -> Text -> m ()
zen dia msg = liftIO $ zenity def (dia def{ text = Just msg })
