{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module Main (main) where

import ClassyPrelude
-- import Control.Lens
import Data.Attoparsec.ByteString.Char8 as Atto hiding (take)
import Data.Char (ord)
import Data.Coerce
import Prelude ()
import Turtle (ExitCode(..))
import Turtle.Prelude

import Data.Csv as Csv
import qualified Turtle.Bytes as Bytes

newtype Pool = Pool Text
  deriving (Eq, Show, IsString)

newtype Path = Path Text
  deriving (Eq, Show, IsString)

newtype Snapshot = Snapshot Text
  deriving (Eq, Show, IsString)

data Dataset = Dataset
  { pool :: Pool
  , path :: Maybe Path
  , snapshot :: Maybe Snapshot
  } deriving (Eq, Show)

toText :: Dataset -> Text
toText x = coerce (pool x) <> write "/" (path x) <> write "@" (snapshot x)
  where write c s = fromMaybe "" ((c <>) . coerce <$> s)

instance ToField Dataset where
  toField = encodeUtf8 . toText

instance FromField Dataset where
  parseField field = either fail pure $ parseOnly parseDataset field

parseDataset :: Atto.Parser Dataset
parseDataset = do
  pool <- Pool . decodeUtf8 <$> takeWhile1 isDatasetChar
  path <- optional $
    char '/' *> (Path . decodeUtf8 <$> takeWhile1 (isDatasetChar <||> (=='/')))
  snapshot <- optional $
    char '@' *> (Snapshot . decodeUtf8 <$> takeWhile1 isDatasetChar)
  return Dataset{..}

isDatasetChar :: Char -> Bool
isDatasetChar = isDigit <||> isSpace <||> isAlpha_ascii <||> inClass "-_.:"

data SnapshotRecord = SnapshotRecord
  { dataset :: Dataset
  , size :: Int
  } deriving (Show, Eq, Generic)

instance FromRecord SnapshotRecord
instance ToRecord SnapshotRecord

decodeTsv :: FromRecord a => HasHeader -> LByteString -> Either String (Vector a)
decodeTsv = decodeWith defaultDecodeOptions{ decDelimiter = word '\t' }

word :: Char -> Word8
word = fromIntegral . ord

exitWhenFail :: ExitCode -> IO ()
exitWhenFail c@(ExitFailure _) = exit c
exitWhenFail _ = pure ()

zfs :: Text -> IO ByteString
zfs args = do
  (c, b) <- Bytes.procStrict "zfs" (words args) mzero
  exitWhenFail c
  return b

zfs_ :: Text -> IO ()
-- zfs_ = say
zfs_ args = proc "zfs" (words args) mzero >>= exitWhenFail

destroyDataset :: Dataset -> IO ()
destroyDataset d = zfs_ $ "destroy " <> toText d

zfsList :: IO ByteString
zfsList = zfs "list -t snapshot -Hpo name,used"

-- zfsList :: IO ByteString
-- zfsList = readFile "zfsList.tsv"

getSnapshotRecords :: IO (Vector SnapshotRecord)
getSnapshotRecords = (decodeTsv NoHeader . fromStrict <$> zfsList) >>= \case
    Left s -> die $ fromString s
    Right v -> return v

isSnapshotInfix :: Text -> Dataset -> Bool
isSnapshotInfix x Dataset{ snapshot = Just (Snapshot s) } = x `isInfixOf` s
isSnapshotInfix _ _ = False

main :: IO ()
main = do
  vsr <- getSnapshotRecords
  let autoRecs = filter (isSnapshotInfix "zfs-auto-snap" . dataset) vsr
  forM_ (groupAllOn ((pool &&& path) . dataset) autoRecs) $ \r -> do
    let emptyBeforeLast = filter ((==0) . size) $ initDef r
    mapM_ (destroyDataset . dataset) emptyBeforeLast
  -- putStr $ decodeUtf8 $ toStrict $ encode $ toList vsr
  -- Bytes.stdout (pure ll)
  return ()
