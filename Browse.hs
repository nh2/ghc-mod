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

import Debug.Trace

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
    -- getInfo :: GhcMonad m => Name -> m (Maybe (TyThing, Fixity, [Instance]))
    case modinfo of
      Nothing -> error "niklas"
      Just mi -> do let exports = modInfoExports mi
                        tythings = modInfoTyThings mi
                        toplevel = modInfoTopLevelScope mi
                    -- trace ("   " ++ showOutputable exports) $ return ()
                    bla <- getInfo (exports !! 20)
                    t <- exprType "(\\x -> x)"
                    -- Useful: idType :: Id -> Kind
                    -- trace ("xx " ++ showOutputable t) $ return ()
                    -- trace ("yy " ++ showOutputable bla) $ return ()
                    -- trace ("tythings " ++ showOutputable tythings) $ return ()
                    -- trace ("toplevel " ++ showOutputable toplevel) $ return ()
                    -- trace ("lasttything " ++ showOutputable lasttything) $ return ()
                    return ()
    -- return $ maybeNamesToStrings modinfo
    case modinfo of
      Nothing -> return []
      Just mi -> do
        case modInfoTopLevelScope mi of
          Nothing -> return []
          Just toplevel -> mapM (getNameAndType mi) toplevel
  where
    render name typ = getOccString name ++ " :: " ++ pretty typ
    getNameAndType modinfo name = do
      tyThing <- modInfoLookupName modinfo name
      return $ case tyThing of
        Just (AnId i) -> render name (idType i)
        Just (ADataCon _datacon) -> "ADataCon " ++ render name (dataConUserType _datacon)
        Just (ATyCon _tycon)     -> "ATyCon " ++ getOccString name -- render name (synTyConType _tycon)
        Just (ACoAxiom _coaxiom) -> "ACoAxiom " ++ getOccString name
        Nothing -> "no tyThing " ++ getOccString name

    -- lookupModuleInfo = findModule (mkModuleName mdlName) Nothing >>= getModuleInfo
    modm = findModule (mkModuleName mdlName) Nothing
    lookupModuleInfo = modm >>= getModuleInfo
    maybeNamesToStrings = maybe [] (map getOccString . modInfoExports)
    -- maybeNamesToStrings = maybe [] (\x -> trace (" asdf" ++ pmodinfo x) $ map getOccString (modInfoExports x))

showOutputable :: (Outputable a) => a -> String
showOutputable = showSDoc . ppr

pmodinfo :: ModuleInfo -> String
pmodinfo mod = let Just modiface = modInfoIface mod in showOutputable (mi_exports modiface)

pretty :: Type -> String
pretty = showDocWith OneLineMode . Gap.styleDoc (mkUserStyle neverQualify AllTheWay) . pprTypeForUser False
