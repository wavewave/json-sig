import JSONSIG
import Data.Aeson
import qualified Data.Attoparsec as Atto
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
    Right o ->  putStrLn (prettyPrint o) -- writeFile (args !! 1) (prettyPrint o)

    -- (makeObjs o)

{-
    (ObjectMap omap) -> do
      let lst = HM.toList omap
          o1 = head lst
          oname = (haskObjName . T.unpack . fst) o1
          lst' = (HM.toList . map_method . snd) o1
      mapM_ (print . haskMethodName oname . T.unpack . fst) lst' 
      -- (print . haskObjName . T.unpack . fst . head ) lst 
  -}
  
  
