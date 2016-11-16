import Data.Time.Clock.POSIX
import Data.Time.Format.Human

main :: IO ()
main = putStrLn =<< humanReadableTime . posixSecondsToUTCTime . fromInteger =<< readLn
