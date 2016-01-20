{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module JSONSIG where

import           Data.Aeson hiding (String)
import qualified Data.Aeson as Aeson (Value(String))
import           Data.Aeson.Types (typeMismatch,Parser)
import qualified Data.ByteString.Char8 as B
import           Data.Char (toLower,toUpper)
import qualified Data.Foldable as F
import qualified Data.HashMap.Strict as HM
import           Data.List (intersperse)
import           Data.List.Split (splitOn)
import qualified Data.Text as T
import qualified Data.Traversable as Tr
import           Data.Vector as V hiding ((++))
import           Language.Haskell.Exts.Syntax
import qualified Language.Haskell.Exts.Pretty as PP


type ObjectName = T.Text
type MethodName = T.Text

newtype ObjectMap = ObjectMap { list_object :: Vector (ObjectName,MethodMap) }
                  deriving (Show)
 
newtype MethodMap = MethodMap { list_method :: Vector (MethodName,MethodSig) }
                  deriving (Show)

newtype MethodSig = MethodSig { list_args :: Vector SigArg }
                  deriving (Show)


data SigPrim = SPInt
             | SPByte
             deriving (Show)

data SigArg = SAPrim SigPrim
            | SAArr  SigPrim
            deriving (Show)




instance FromJSON ObjectMap where
  parseJSON (Array vs) = do
    let f (Object v) = do ms <- parseJSON =<< (v .: "methods")
                          (,) <$> v .: "name" <*> pure ms
        f inv = typeMismatch "ObjectMap: not object" inv
    vs' <- Tr.mapM f vs 
    return (ObjectMap vs')
  parseJSON invalid = typeMismatch "ObjectMap: not array" invalid

 
instance FromJSON MethodMap where
  parseJSON (Array vs) = do
    let f (Object v) = do as <- parseJSON =<< (v .: "args")
                          (,) <$> v .: "name" <*> pure as
        f inv = typeMismatch "MethodMap: not object" inv
    vs' <- Tr.mapM f vs 
    return (MethodMap vs')
  parseJSON invalid = typeMismatch "MethodMap: not array" invalid

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

tapp = TyApp

tcon = TyCon . UnQual . Ident

promoInt = TyPromoted . PromotedInteger

promoList = TyPromoted . PromotedList True

promoCon = PromotedCon False . UnQual . Ident
           
makeMethod objname o (m,(mtd,args)) = TypeDecl src mtdident [] typ
  where mtdname =  haskMethodName objname (T.unpack mtd)
        mtdident = Ident mtdname
        arglist = (V.toList . list_args) args
        argtyps = fmap makeArg arglist
        typ = (tcon "Sig") `tapp` (promoInt o) `tapp` (promoInt m) `tapp` (promoList argtyps)


makeObjDecl (o,(txt,ms)) = declObj : declMethods
  where objname = (haskObjName . T.unpack) txt
        objident = Ident objname
        declObj = DataDecl src DataType [] objident [] [] []
        mths = (Prelude.zip [0..] . V.toList . list_method) ms
        declMethods = fmap (makeMethod objname o) mths

makeObjs (ObjectMap omap) = F.concatMap makeObjDecl lst
  where lst = (Prelude.zip [0..] . V.toList) omap

createModule :: ModuleName -> ObjectMap -> Module
createModule mod o = Module src mod pragmas Nothing Nothing imports decls
  where
    pragmas = [ LanguagePragma src (fmap Ident [ "DataKinds", "EmptyDataDecls", "GADTs", "KindSignatures", "TypeOperators" ]) ]
    imports = [ ImportDecl src (ModuleName "GHC.TypeLits") False False False Nothing Nothing Nothing
              , ImportDecl src (ModuleName "Android.Bridge.Type") False False False Nothing Nothing Nothing ]
    decls = makeObjs o


style = PP.Style PP.PageMode 132 0.6

myMode :: PP.PPHsMode
myMode = PP.PPHsMode 2 2 2 2 2 4 1 True PP.PPOffsideRule False 

prettyPrint mod o = PP.prettyPrintStyleMode style myMode (createModule mod o)
