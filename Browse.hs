module Browse (browseModule) where

import Control.Applicative
import Data.Char
import Data.List
import GHC
import Outputable
import GHCApi
import Name
import Types
import PprTyThing
import Pretty (showDocWith, Mode(OneLineMode))
import qualified Gap

----------------------------------------------------------------

browseModule :: Options -> String -> IO String
browseModule opt mdlName = convert opt . format <$> browse opt mdlName
  where
    format
      | operators opt = formatOps
      | otherwise     = removeOps
    removeOps = sort . filter (isAlpha.head)
    formatOps = sort . map formatOps'
    formatOps' x@(s:_)
      | isAlpha s = x
      | otherwise = '(' : x ++ ")"
    formatOps' [] = error "formatOps'"

browse :: Options -> String -> IO [String]
browse opt mdlName = withGHC $ do
    _ <- initSession0 opt
    modinfo <- lookupModuleInfo
    maybe (return []) getNamesWithTypes modinfo
  where
    getNamesWithTypes modinfo = maybe (return []) (mapM (getNameWithType modinfo)) $ modInfoTopLevelScope modinfo

    getNameWithType modinfo name = do
      tyThing <- modInfoLookupName modinfo name
      return $ case tyThing of
        Just (AnId i)            -> render name (idType i)
        Just (ADataCon _datacon) -> "ADataCon " ++ render name (dataConUserType _datacon)
        Just (ATyCon _tycon)     -> "ATyCon " ++ getOccString name -- render name (synTyConType _tycon)
        Just (ACoAxiom _coaxiom) -> "ACoAxiom " ++ getOccString name
        Nothing                  -> "no tyThing " ++ getOccString name

    lookupModuleInfo = findModule (mkModuleName mdlName) Nothing >>= getModuleInfo
    render name typ = getOccString name ++ " :: " ++ pretty typ

pretty :: Type -> String
pretty = showDocWith OneLineMode . Gap.styleDoc (mkUserStyle neverQualify AllTheWay) . pprTypeForUser False
