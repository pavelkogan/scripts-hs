{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoImplicitPrelude #-}

import BasicPrelude hiding (FilePath, empty)
import System.IO.Unsafe
import Turtle hiding (text)
import Zenity

type Tag = Text

parser :: Parser [FilePath]
parser = some $ argPath "files" "files to tag"

main :: IO ()
main = do
  args <- options "tmsu-add-tags" parser
  shouldFollowLinks <- not <$> inTaggedTree (head args)
  files <- if shouldFollowLinks then followLinks args
           else return args
  tags <- getTags
  addTags tags files

followLinks :: MonadIO m => [FilePath] -> m [FilePath]
followLinks links = do
  paths <- catMaybes <$> traverse readlink links
  files <- filterM testfile paths
  return files

readlink :: MonadIO m => FilePath -> m (Maybe FilePath)
readlink link = do
  let linkText = fromString $ encodeString link
  (_, ret) <- procStrict "readlink" ["-q", linkText] empty
  let mpath = map fromText $ listToMaybe $ lines ret
  return mpath

getTags :: IO [Tag]
getTags = do
  let dialog = Entry def{ text = Just "Enter tags to add:" }
  mentry <- zenity def{ title = Just "Add tags" } dialog
  let entry = maybeToList mentry
  return $ words =<< entry

addTags :: MonadIO m => [Tag] -> [FilePath] -> m ()
addTags tags files = inDir (parent (head files)) $ do
  let tagStr = unwords tags
  let args = ["tag", "--tags=\""<>tagStr<>"\""] ++ (toText' <$> files)
  (ec, sout, serr) <- procStrictWithErr "tmsu" args empty
  case ec of
    ExitSuccess -> unless (sout == mempty) $ notifyMsgIO sout
    ExitFailure _ -> warnMsgIO serr

inDir :: MonadIO m => FilePath -> m a -> m a
inDir newDir action = do
  oldDir <- pwd
  cd newDir
  ret <- action
  cd oldDir
  return ret

inTaggedTree :: MonadIO m => FilePath -> m Bool
inTaggedTree file = inDir (parent file) $ do
  (ec, _, _) <- procStrictWithErr "tmsu" ["info"] empty
  return $ ec == ExitSuccess

toText' :: FilePath -> Text
toText' = either (errorMsg "Failed to decode FilePath") id . toText

errorMsg :: Text -> a
errorMsg msg = unsafePerformIO $ zenity def (Error def{ text = Just msg }) >> undefined

warnMsgIO, notifyMsgIO :: MonadIO m => Text -> m ()
warnMsgIO msg = liftIO $ zenity def (Warning def{ text = Just msg })
notifyMsgIO msg = liftIO $ zenity def (Notification def{ text = Just msg })
