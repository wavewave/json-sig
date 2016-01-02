import JSONSIG
import Data.Aeson
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as LB
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import System.Environment

main :: IO ()
main = do
  args <- getArgs
  bstr <- LB.readFile (args !! 0)
  
  case (eitherDecode bstr :: Either String ObjectMap) of
    Left str -> putStrLn str
    Right o ->  writeFile (args !! 1) (prettyPrint o)
  
