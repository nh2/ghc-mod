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
    maybe (return []) getNamesWithTypes =<< lookupModuleInfo
  where
    lookupModuleInfo = findModule (mkModuleName mdlName) Nothing >>= getModuleInfo

    getNamesWithTypes modinfo = mapM (getNameWithType modinfo) $ modInfoExports modinfo

    getNameWithType modinfo name = do
      -- If we don't call getInfo, tyThing will be nothing for many functions. Why?
      _ <- getInfo name

      tyThing <- modInfoLookupName modinfo name

      return $ case tyThing of
        Just (AnId i)            -> renderWithType name (idType i)
        Just (ADataCon datacon)  -> renderWithType name (dataConUserType datacon)
        Just (ATyCon tycon)      -> renderWithType name (synTyConResKind tycon)
        Just (ACoAxiom _coaxiom) -> getOccString name
        Nothing                  -> getOccString name

    renderWithType name typ = getOccString name ++ " :: " ++ pretty typ

-- Copied from Info.hs
pretty :: Type -> String
pretty = showDocWith OneLineMode . Gap.styleDoc (mkUserStyle neverQualify AllTheWay) . pprTypeForUser False
