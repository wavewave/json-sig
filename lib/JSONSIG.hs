{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module JSONSIG where

import           Data.Aeson hiding (String)
import qualified Data.Aeson as Aeson (Value(String))
import           Data.Aeson.Types (typeMismatch)
-- import qualified Data.Attoparsec as A
import qualified Data.ByteString.Char8 as B
import           Data.Char (toLower,toUpper)
import qualified Data.Foldable as F
import qualified Data.HashMap.Strict as HM
import           Data.List (intersperse)
import           Data.List.Split (splitOn)
import qualified Data.Text as T
import qualified Data.Traversable as Tr
import           Data.Vector hiding ((++))
import           Language.Haskell.Exts.Syntax
import qualified Language.Haskell.Exts.Pretty as PP

-- parseSig :: B.ByteString -> A.Result Value
-- parseSig str = A.parse json str 

type ObjectName = T.Text
type MethodName = T.Text

newtype ObjectMap = ObjectMap { map_object :: HM.HashMap ObjectName MethodMap }
                  deriving (Show)

newtype MethodMap = MethodMap { map_method :: HM.HashMap MethodName MethodSig }
                  deriving (Show)

newtype MethodSig = MethodSig { method_args :: Vector SigArg }
                  deriving (Show)

data SigPrim = SPInt
             | SPByte
             deriving (Show)

data SigArg = SAPrim SigPrim
            | SAArr  SigPrim
            deriving (Show)




instance FromJSON ObjectMap where
  parseJSON (Object v) = do
    v' <- Tr.mapM parseJSON v
    return (ObjectMap v')
  parseJSON invalid = typeMismatch "ObjectMap: not object" invalid

instance FromJSON MethodMap where
  parseJSON (Object v) = do
    v' <- Tr.mapM parseJSON v
    return (MethodMap v')
  parseJSON invalid = typeMismatch "MethodMap: not object" invalid

instance FromJSON MethodSig where
  parseJSON (Array vs) = do
    vs' <- Tr.mapM parseJSON vs
    return (MethodSig vs')
  parseJSON invalid = typeMismatch "MethodSig: not array" invalid


instance FromJSON SigArg where
  parseJSON x@(Aeson.String txt) =
    case parseArg txt of
      Left str -> typeMismatch str x
      Right v -> return v
  parseJSON invalid = typeMismatch "SigArg: not string" invalid

parseArg :: T.Text -> Either String SigArg
parseArg txt = case T.uncons txt of
                 Nothing -> Left "parseArg: null string"
                 Just (x,rest) ->
                   case parsePrim x of
                     Just p -> Right (SAPrim p)
                     Nothing -> case x of
                       '[' -> case T.uncons rest of
                                Nothing -> Left ("parseArg: not valid array (Nothing) " ++ T.unpack txt)
                                Just (y,_) -> case parsePrim y of
                                                Just p' -> Right (SAArr p')
                                                Nothing -> Left ("parseArg: not valid array " ++ T.unpack txt)
                       _ -> Left "parseArg: not a valid first charactor"

parsePrim :: Char -> Maybe SigPrim
parsePrim c = case c of
                'I' -> Just SPInt
                'B' -> Just SPByte
                _ -> Nothing


haskObjName :: String -> String
haskObjName str = headToUpper (F.concat (intersperse "_" strs))
  where strs = splitOn "." str  


haskMethodName :: String -> String -> String
haskMethodName obj method = obj ++ "_" ++ method

headToUpper (x:xs) = toUpper x : xs
headToUpper _ = error "headToUpper"

headToLower (x:xs) = toLower x : xs
headToLower _ = error "headToLower"

src :: SrcLoc
src = SrcLoc "No SrcLoc" 0 0

local :: String -> QName
local t = UnQual (Ident t)

lcon :: String -> Exp
lcon = Con . local



makeArg :: SigArg -> Type
makeArg (SAPrim p) = makePrim p
makeArg (SAArr p) = TyApp (TyCon (local "AArr")) (makePrim p)

makePrim :: SigPrim -> Type
makePrim SPInt = TyCon (local "PInt")
makePrim SPByte = TyCon (local "PByte")

makeMethod objname (mtd,args) = QualConDecl src [] [] (ConDecl mtdident typs)
  where mtdname =  haskMethodName objname (T.unpack mtd)
        mtdident = Ident mtdname
        arglist = (F.toList . method_args) args
        typs = fmap makeArg arglist

makeObjDecl (txt,ms) = DataDecl src DataType [] objident [] values []
  where objname = (haskObjName . T.unpack) txt
        objident = Ident objname
        values = (fmap (makeMethod objname) . HM.toList . map_method) ms

makeObjs (ObjectMap omap) = fmap makeObjDecl lst
  where lst = HM.toList omap

createModule :: ObjectMap -> Module
createModule o = Module src (ModuleName "Signature") [] Nothing Nothing [] decls
  where decls = makeObjs o


style = PP.Style PP.PageMode 132 0.6

myMode :: PP.PPHsMode
myMode = PP.PPHsMode 2 2 2 2 2 4 1 True PP.PPOffsideRule False 

prettyPrint o = PP.prettyPrintStyleMode style myMode (createModule o)
