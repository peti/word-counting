import Control.Exception        ( handleJust )
import Control.Monad            ( guard, liftM )
import System.Environment       ( getArgs, withArgs )
import System.IO                ( Handle, IOMode(..), withBinaryFile, stdout, hShow )
import System.IO.Error          ( isEOFError )
import Data.ByteString.Char8    ( ByteString, hGetLine, hPutStrLn, pack, unpack )
import qualified Data.ByteString.Char8 as Str
import Data.Array
import Text.Regex.Posix

main :: IO ()
main = getArgs >>= mapM_ (processLogfile (hPutStrLn stdout) lineHandlers)
  where
    lineHandlers =
      [  handle "sshd\\[[0-9]+\\]: accepted publickey for ([^ ]+) from ([^ ]+)"
                (Str.intercalate (pack " "))
      ]

data LineHandler = Handler Regex ([ByteString] -> ByteString)

handle :: String -> ([ByteString] -> ByteString) -> LineHandler
handle guard action = Handler (makeRegexOpts (compExtended + compIgnoreCase) execBlank guard) action

processLogfile :: (ByteString -> IO ()) -> [LineHandler] -> FilePath -> IO ()
processLogfile resultHandler lineHandler path =
  withBinaryFile path ReadMode $ \h ->
    handleJust (guard . isEOFError) return (ioloop h)
      where ioloop h = hGetLine h >>= processLine resultHandler lineHandler >> ioloop h

processLine :: (ByteString -> IO ())  -> [LineHandler] -> ByteString -> IO ()
processLine resultHandler handlers line = maybe (return ()) (resultHandler) (matchLines handlers line)
  where
    matchLines [] _        = Nothing
    matchLines (h:hs) line = maybe (matchLines hs line) (Just) (matchLine h line)

    matchLine :: LineHandler -> ByteString -> Maybe ByteString
    matchLine (Handler guard action) line =
      maybe Nothing (\(_,arr,_) -> Just (action (map fst (tail (elems arr))))) (matchOnceText guard line)

    extractDate line = let (monthName:day:timeOfDay:_) = Str.words line
                           months = ["Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"]
                           monthNames = zip (map pack months) (map (pack . show) [1..12])
                           Just month = lookup monthName monthNames
                       in
                         Str.intercalate (pack " ")
                            [ Str.intercalate (pack "-") [pack "2010", month, day]
                            , timeOfDay
                            ]
