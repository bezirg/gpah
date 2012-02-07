module Analysis.Function.Base where

import Analysis.Utils
import qualified Data.Map as M


type Import  = String

type FavFunction = String                                     -- Function-name to search for
type OneLvlUp    = String                                     -- Function one level up from where a match was found

type Occs        = Int                                        
type Analysis    = M.Map FavFunction (M.Map ModuleFileName Occs) -- the f-analysis is a mapping of function to search for to a mapping of module-name 
                                                                 -- and the nr of occurences in that module


type FuncInfo    = [(FavFunction,LineNumber,OneLvlUp,Int)]    -- fav-function, line number, func one level up and the nr of times a match is found in the expression


-- | Functions to search for sorted per module
favorites :: M.Map String [String]
favorites = 
 M.fromList
 [("Data.Generics.Aliases"            , ["mkT", "mkQ", "mkM", "mkMp", "mkR", "ext0", "extT", "extQ", "extM", "extMp", "extB", "extR", "orElse", "recoverMp", "recoverQ", "choiceMp", "choiceQ",  "ext1", "ext1T", "ext1M", "ext1Q", "ext1R", "ext1B",  "ext2T", "ext2M", "ext2Q", "ext2R", "ext2B"]),
  ("Data.Generics.Schemes"            , ["everywhere","everywhere'","everywhereBut","everywhereM","somewhere","everything","everythingBut","listify","something","synthesize","gsize","glength","gdepth","gcount","gnodecount","gtypecount","gfindtype"]),
  ("Data.Generics.Text"               , ["gshow", "gshows", "gread"]),
  ("Data.Generics.Twins"              , ["gfoldlAccum","gmapAccumT","gmapAccumM","gmapAccumQl","gmapAccumQr","gmapAccumQ","gmapAccumA","gzipWithT","gzipWithM","gzipWithQ","geq","gzip"]),
  ("Data.Generics.Builders"           , ["empty", "constrs"]),
  ("Generics.SYB.Aliases"             , ["mkT", "mkQ", "mkM", "mkMp", "mkR", "ext0", "extT", "extQ", "extM", "extMp", "extB", "extR", "orElse", "recoverMp", "recoverQ", "choiceMp", "choiceQ",  "ext1", "ext1T", "ext1M", "ext1Q", "ext1R", "ext1B",  "ext2T", "ext2M", "ext2Q", "ext2R", "ext2B"]),
  ("Generics.SYB.Schemes"             , ["everywhere","everywhere'","everywhereBut","everywhereM","somewhere","everything","everythingBut","listify","something","synthesize","gsize","glength","gdepth","gcount","gnodecount","gtypecount","gfindtype"]),
  ("Generics.SYB.Text"                , ["gshow", "gshows", "gread"]),
  ("Generics.SYB.Twins"               , ["gfoldlAccum","gmapAccumT","gmapAccumM","gmapAccumQl","gmapAccumQr","gmapAccumQ","gmapAccumA","gzipWithT","gzipWithM","gzipWithQ","geq","gzip"]),
  ("Generics.SYB.Builders"            , ["empty", "constrs"]),
  ("Data.Data"                        , ["gfoldl","gunfold","toConstr", "dataTypeOf","dataCast1", "dataCast2", "gmapT","gmapQ","gmapQl","gmapQr","gmapQi","gmapM","gmapMp","gmapMo", "mkDataType","mkIntType", "mkFloatType","mkStringType", "mkCharType",   "mkNoRepType",  "mkNorepType",  "dataTypeName","dataTypeRep","repConstr",  "isAlgType",  "dataTypeConstrs","indexConstr",    "maxConstrIndex", "isNorepType","mkConstr",       "mkIntConstr",    "mkFloatConstr",  "mkIntegralConstr","mkRealConstr",   "mkStringConstr", "mkCharConstr",   "constrType",     "ConstrRep",  "constrRep",      "constrFields",   "constrFixity",   "constrIndex",    "showConstr",     "readConstr",     "tyconUQname",    "tyconModule",    "fromConstr",     "fromConstrB",    "fromConstrM"]),
  ("Data.Typeable"                    , ["mkTyCon","mkTyCon3","mkTyConApp","mkAppTy","typeRepTyCon","typeOfDefault","typeOf1Default","typeOf2Default","typeOf3Default","typeOf4Default","typeOf5Default","typeOf6Default","mkFunTy","splitTyConApp","funResultTy","typeRepArgs","showsTypeRep","tyConString","listTc","funTc"]),
  ("Data.Generics.Uniplate.Operations", ["uniplate","descend","descendM","biplate","descendBi","descendBiM","universe","children","transform","transformM","rewrite","rewriteM","contexts","holes","para","universeBi","childrenBi","transformBi","transformBiM","rewriteBi","rewriteBiM","contextsBi","holesBi"]),
  ("Data.Generics.Uniplate.Direct"    , ["plate", "plateSelf", "|+", "|-", "|*", "||+", "||*", "plateProject"]),
  ("Data.Generics.Uniplate.Zipper"    , ["zipper", "zipperBi", "fromZipper","left", "right","up", "down","hole","replaceHole"]),
  ("Data.Generics.Uniplate.Data"      , ["transformBis", "transformer"]),
  ("Data.Generics.SYB"                , ["gmapT", "gmapQl", "gmapQr", "gmapQ", "gmapQi", "gmapM", "mkT", "everywhere", "mkM", "everywhereM", "mkQ", "everything"]),
  ("Data.Generics.Compos"             , ["composOp", "composOpM", "composOpM_", "composOpMonoid", "composOpMPlus", "composOpFold"])
 ]
 
