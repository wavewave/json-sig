import JSONSIG
import Data.Aeson
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as LB
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import           Language.Haskell.Exts.Syntax (ModuleName(..))
import System.Environment
import System.FilePath (takeBaseName)

main :: IO ()
main = do
  args <- getArgs
  let ifile = args !! 0
      ofile = args !! 1
      modname = takeBaseName ofile
      mod = ModuleName modname
  bstr <- LB.readFile ifile
  case (eitherDecode bstr :: Either String ObjectMap) of
    Left str -> putStrLn str
    Right o ->  writeFile ofile (prettyPrint mod o)
  
