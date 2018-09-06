{-# LANGUAGE RecordWildCards #-}

import Control.Monad
import Control.Monad.Extra
import Data.Maybe
import Prelude hiding (FilePath)
import Turtle

data Options = Options
  { destination :: Maybe FilePath
  , sources :: [FilePath]
  } deriving (Show, Eq)

parser :: Parser Options
parser = Options
  <$> optional (optPath "destination" 'd' "Destination directory")
  <*> some (argPath "sources" "Source files or directories")

main :: IO ()
main = do
  Options{..} <- options "mvt" parser
  dest <- fromMaybe <$> pwd <*> pure destination
  unlessM (testdir dest) $ die "Destination directory not found"
  forM_ sources $ \src ->
    unlessM (testpath src) $
      die $ lineToText ("'" <> showPath src <> "' not found")
  unlessM (testdir $ dest </> ".torrent") $ mkdir (dest </> ".torrent")
  fails <- sequence $ mvt <$> sources <*> pure dest
  forM_ (catMaybes fails) $ \a -> err $ "File missing: " <> a

-- | Moves file and associated .torrent file, returning Nothing.
-- If .torrent file doesn't exist, returns Just that file's path.
mvt :: MonadIO m => FilePath -> FilePath -> m (Maybe Line)
mvt src dest = do
  let tf = parent src </> ".torrent" </> name src <.> "torrent"
  ifM (not <$> testfile tf) (return $ Just $ showPath tf) $ do
    mv src (dest </> name src)
    mv tf (dest </> ".torrent" </> filename tf)
    echo $ "Moved: " <> showPath (name src)
    return Nothing

name :: FilePath -> FilePath
name p = if p == directory p then dirname p else filename p

showPath :: FilePath -> Line
showPath = either unsafeTextToLine unsafeTextToLine . toText
