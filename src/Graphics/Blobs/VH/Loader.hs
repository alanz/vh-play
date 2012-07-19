{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
module Graphics.Blobs.VH.Loader
       (
         getPage
       , getFiles
       , getAllPages
       , getModuleGraphBasis
       ) where

{-

This file is based initially on Language.Haskell.BuildWrapper.CMD

https://github.com/JPMoresmau/BuildWrapper/blob/master/src-exe/Language/Haskell/BuildWrapper/CMD.hs

-}

-- import Paths_buildwrapper
-- import qualified MonadUtils as GMU
import Control.Monad.State
import Data.Aeson
import Data.Version (showVersion)
import Language.Haskell.BuildWrapper.Base hiding (tempFolder,cabalPath, cabalFile, cabalFlags,verbosity)
import Language.Haskell.BuildWrapper.Cabal
import Language.Haskell.BuildWrapper.GHC
import Language.Haskell.Exts.Parser
import System.Console.CmdArgs hiding (Verbosity(..),verbosity)
import Language.Haskell.Exts.Annotated.Syntax
import Language.Haskell.Exts.SrcLoc
import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Lazy.Char8 as BSC
import qualified Language.Haskell.BuildWrapper.API as BW
{-
import qualified GHC                   as GHC
import qualified DynFlags              as GHC
import qualified Outputable            as GHC
import qualified MonadUtils            as GHC
import qualified NameSet               as GHC
import qualified HsLit                 as GHC
import GHC.Paths ( libdir )
-}

type CabalFile = FilePath
type CabalPath = FilePath
type TempFolder = FilePath

{-
runCmd :: (ToJSON a) => BWCmd -> StateT BuildWrapperState IO a -> IO ()
runCmd = runCmdV Normal
-}
{-
runCmdV:: (ToJSON a) => Verbosity -> BWCmd -> StateT BuildWrapperState IO a -> IO ()
runCmdV vb cmd f =
  evalStateT f (BuildWrapperState (tempFolder cmd) (cabalPath cmd) (cabalFile cmd) vb (cabalFlags cmd) (cabalOption cmd))
  >>= BSC.putStrLn . BS.append "build-wrapper-json:" . encode
-}

runCmdVt :: Monad m => StateT BuildWrapperState m a -> m a
runCmdVt f = evalStateT f bwState

bwState  = BuildWrapperState
  {- tempFolder  = -} ctempFolder
  {- , cabalPath   = -} ccabalPath
  {- , cabalFile   = -} ccabalFile
  {- , verbosity   = -} Normal
  {- , cabalFlags  = -} ccabalFlags
  {- , cabalOpts   = -} ccabalOption





ctempFolder :: TempFolder
ctempFolder  = ".dist-buildwrapper"

ccabalPath :: CabalPath
ccabalPath   = "cabal"

ccabalFile :: CabalFile
ccabalFile   = "vh-play.cabal"

ccabalFlags :: String
ccabalFlags  = ""

ccabalOption :: [String]
ccabalOption = []

sync :: IO ()
sync = runCmdVt (BW.synchronize True)
  >>= BSC.putStrLn . BS.append "build-wrapper-json:" . encode


outline' :: FilePath -> IO (OpResult OutlineResult)
outline' filePath = runCmdVt (BW.getOutline filePath)


getPage :: FilePath -> IO (String, [OutlineDef])
getPage filePath = do
  (res, _notes) <- outline' filePath
  -- modulename <- getModuleInfo' filePath
  modulename <- getModuleName filePath
  return (modulename, (orOutline res))

-- ---------------------------------------------------------------------

getModuleName :: FilePath -> IO (String)
getModuleName filePath = do
  (maybeParse,_notes) <- runCmdVt (BW.getAST filePath)
  case maybeParse of
    (Just x) ->  do
      let (Module _ll modulehead _pragmas _imports _decls,_comments) = (fromParseResult x)
      case modulehead of
        Just (ModuleHead _l' (ModuleName _l modulename) _maybeWarning _maybeExports) ->  return modulename
        Nothing -> return ("empty head:" ++ filePath)
      -- return x
    Nothing       -> return ("parse fail:" ++ filePath)

-- ---------------------------------------------------------------------

getFilesBw :: BuildWrapper(OpResult [FilePath])
getFilesBw = do
        -- cf < - gets cabalFile
        (fileList,ns)<-getFilesToCopy
        return ((fileList), ns)

getFiles :: IO [FilePath]
getFiles = do
  (files,_notes) <- runCmdVt (getFilesBw)
  return files

-- ---------------------------------------------------------------------

getAllPages :: IO [(String, [OutlineDef])]
getAllPages = do
  files <- getFiles
  m1 <- mapM getPage files
  return m1

-- ---------------------------------------------------------------------

getModuleInfo filePath = do
  (maybeParse,_notes) <- runCmdVt (BW.getAST filePath)
  case maybeParse of
    (Just x) ->  do
      let (Module _ll modulehead _pragmas _imports _decls,_comments) = (fromParseResult x)
      case modulehead of
        Just (ModuleHead _l' (ModuleName _l modulename) _maybeWarning _maybeExports) ->  return modulename
        Nothing -> return ("empty head:" ++ filePath)
      -- return x
    Nothing       -> return ("parse fail:" ++ filePath)

-- ---------------------------------------------------------------------

getModuleGraphBasis = do
  files <- getFiles
  m1 <- mapM getModuleInfo files
  return m1

-- ---------------------------------------------------------------------

getModuleNameFromModuleHead
  :: String -> Maybe (ModuleHead t) -> String
getModuleNameFromModuleHead filePath modulehead =
  case modulehead of
    Just (ModuleHead _l' (ModuleName _l modulename) _maybeWarning _maybeExports) ->  modulename
    Nothing -> ("empty head:" ++ filePath)

-- ---------------------------------------------------------------------

getDeclDetails :: Decl SrcSpanInfo -> [String]
getDeclDetails decl =
 case decl of
   TypeDecl    l head typ {-  (DeclHead l) (Type l) -}
     -> ["TypeDecl"] -- A type declaration
   TypeFamDecl l head maybeKind {- (DeclHead l) (Maybe (Kind l)) -}
     -> ["TypeFamDecl"] -- A type family declaration
   DataDecl l don maybeContext head qual maybeDeriving {- (DataOrNew l) (Maybe (Context l)) (DeclHead l) [QualConDecl l] (Maybe (Deriving l)) -}
     -> ["DataDecl"] -- A data OR newtype declaration
   GDataDecl l don maybeContext head maybeKind gadt maybeDeriving {- (DataOrNew l) (Maybe (Context l)) (DeclHead l) (Maybe (Kind l)) [GadtDecl l] (Maybe (Deriving l)) -}
     -> ["GDataDecl"] -- A data OR newtype declaration, GADT style
   DataFamDecl l maybeContext head maybeKind {- (Maybe (Context l)) (DeclHead l) (Maybe (Kind l)) -}
     -> ["DataFamDecl"] -- A data family declaration
   TypeInsDecl l typ1 typ2 {-  (Type l) (Type l) -}
     -> ["TypeInsDecl"] -- A type family instance declaration
   DataInsDecl l don typ qual maybeDeriving {- (DataOrNew l) (Type l) [QualConDecl l] (Maybe (Deriving l)) -}
     -> ["DataInsDecl"] -- A data family instance declaration
   GDataInsDecl l don typ maybeKind gadt maybeDeriving {- (DataOrNew l) (Type l) (Maybe (Kind l)) [GadtDecl l] (Maybe (Deriving l)) -}
     -> ["GDataInsDecl"] -- A data family instance declaration, GADT style
   ClassDecl l maybeContext head fundep maybeClass {- (Maybe (Context l)) (DeclHead l) [FunDep l] (Maybe [ClassDecl l]) -}
     -> ["ClassDecl"] -- A declaration of a type class
   InstDecl l maybeContext head maybeInst {- (Maybe (Context l)) (InstHead l) (Maybe [InstDecl l]) -}
     -> ["InstDecl"] -- An declaration of a type class instance
   DerivDecl l maybeContext inst {- (Maybe (Context l)) (InstHead l) -}
     -> ["DerivDecl"] -- A standalone deriving declaration
   InfixDecl l assoc maybeInt op {- (Assoc l) (Maybe Int) [Op l] -}
     -> ["InfixDecl"] -- A declaration of operator fixity
   DefaultDecl l typ {- [Type l] -}
     -> ["DefaultDecl"] -- A declaration of default types
   SpliceDecl l exp {- (Exp l) -}
     -> ["SpliceDecl"] -- A Template Haskell splicing declaration
   TypeSig l name typ {- [Name l] (Type l) -}
     -> ["TypeSig"] -- A type signature declaration
   FunBind l matches {- [Match l] -}
     -> ["(FunBind:" ++ (concatMap getMatchDetails matches) ++  ")"] -- A set of function binding clauses
   PatBind l pat maybeTyp rhs maybeBinds {- (Pat l) (Maybe (Type l)) (Rhs l) (Maybe (Binds l)) -}
     -> ["(PatBind:" ++ {- (show pat) ++ " "  ++ -} (getDetailsRhs rhs) ++ ")"] -- A pattern binding
   ForImp l callconv maybeSafety maybeString name typ {- (CallConv l) (Maybe (Safety l)) (Maybe String) (Name l) (Type l) -}
     -> ["ForImp"] -- A foreign import declaration
   ForExp l callconv maybeString name typ {- (CallConv l) (Maybe String) (Name l) (Type l) -}
     -> ["ForExp"] -- A foreign export declaration
   RulePragmaDecl l rules {- [Rule l] -}
     -> ["RulePragmaDecl"] -- A RULES pragma
   DeprPragmaDecl l pragmas {- [([Name l], String)] -}
     -> ["DeprPragmaDecl"] -- A DEPRECATED pragma
   WarnPragmaDecl l pragmas {- [([Name l], String)] -}
     -> ["WarnPragmaDecl"] -- A WARNING pragma
   InlineSig l flag maybeActivation name {- Bool (Maybe (Activation l)) (QName l) -}
     -> ["InlineSig"] -- An INLINE pragma
   InlineConlikeSig l maybeActivation name {- (Maybe (Activation l)) (QName l) -}
     -> ["InlineConLikeSig"] -- An INLINE CONLIKE pragma
   SpecSig l name typ {- (QName l) [Type l] -}
     -> ["SpecSig"] -- A SPECIALISE pragma
   SpecInlineSig l flag maybeActivation name typ {- Bool (Maybe (Activation l)) (QName l) [Type l] -}
     -> ["SpecInlineSig"] -- A SPECIALISE INLINE pragma
   InstSig l maybeContext head {- (Maybe (Context l)) (InstHead l) -}
     -> ["IntSig"] -- A SPECIALISE instance pragma
   AnnPragma l annot {- (Annotation l) -}
     -> ["AnnPragma"]

-- ---------------------------------------------------------------------

getDetailsRhs rhs =
  case rhs of
    UnGuardedRhs l exp {- (Exp l) -}
      -> ("UnguardedRhs:" ++ (getDetailsExp exp)) -- unguarded right hand side (exp)
    GuardedRhss l grhss {- [GuardedRhs l] -} -> "GuardedRhss" -- guarded right hand side (gdrhs)

-- ---------------------------------------------------------------------

getMatchDetails match =
  case match of
    Match l name pats rhs maybeBinds {- (Name l) [Pat l] (Rhs l) (Maybe (Binds l)) -}
      -> ("Match:" ++ (getDetailsName name) ++ " " ++ (getDetailsRhs rhs))
         -- A clause defined with prefix notation, i.e. the function
         -- name followed by its argument patterns, the right-hand
         -- side and an optional where clause.
    InfixMatch l pat name pats rhs maybeBinds {- (Pat l) (Name l) [Pat l] (Rhs l) (Maybe (Binds l)) -}
      -> ("InfixMatch:" ++ (getDetailsName name) ++ " " ++ (getDetailsRhs rhs) ++ ")")
         -- A clause defined with infix notation, i.e. first its
         -- first argument pattern, then the function name, then its
         -- following argument(s), the right-hand side and an
         -- optional where clause. Note that there can be more than
         -- two arguments to a function declared infix, hence the
         -- list of pattern arguments.

-- ---------------------------------------------------------------------

getDetailsName name =
  case name of
    Ident l s {- String	-}
      -> ("Ident:" ++ s)
         -- varid or conid.
    Symbol l s {- String -}
      -> ("Symbol:" ++ s)
         -- varsym or consym

-- ---------------------------------------------------------------------

getDetailsQName qname =
  case qname of
    Qual l mname name {- (ModuleName l) (Name l) -}
      -> ("Qual:" ++ (getDetailsModuleName mname) ++ "," ++ (getDetailsName name) ++ ")")
         -- name qualified with a module name
    UnQual l name {- (Name l) -}
      -> ("(UnQual:" ++ (getDetailsName name) ++ ")")
         -- unqualified local name
    Special l sp {- (SpecialCon l) -}
      -> ("Special:")
         -- built-in constructor with special syntax

-- ---------------------------------------------------------------------

getDetailsModuleName (ModuleName l s) = ("(ModuleName:" ++ s ++ ")")

-- ---------------------------------------------------------------------

getDetailsExp exp =
  case exp of
    Var l name {- (QName l) -}
      -> ("Var:" ++ (getDetailsQName name)) -- variable
         -- terminal
    IPVar l ipname {- (IPName l) -}
      -> ("IPVar:" ++ (show ipname)) -- implicit parameter variable
         -- terminal
    Con l name {- (QName l) -}
      -> ("Con:" ++ (getDetailsQName name)) -- data constructor
         -- terminal
    Lit l lit {- (Literal l) -}
      -> ("Lit:)") -- literal constant
         -- terminal
    InfixApp l exp1 op exp2 {- (Exp l) (QOp l) (Exp l) -}
      -> ("InfixApp:" ++ (getDetailsExp exp1) ++ " " ++ (show op) ++ " " ++ (getDetailsExp exp2))
         -- infix application
    App l exp1 exp2 {- (Exp l) (Exp l) -}
      -> ("App:" ++ (getDetailsExp exp1) ++ " " ++ (getDetailsExp exp2))
         -- ordinary application
    NegApp l exp {- (Exp l) -}
      -> ("NegApp:" ++ (getDetailsExp exp))
         -- negation expression -exp (unary minus)
    Lambda l pats exp {- [Pat l] (Exp l) -}
      -> ("Lambda")
         -- lambda expression
    Let l binds exp {- (Binds l) (Exp l) -}
      -> ("Let:" ++ (show binds) ++ " " ++ (getDetailsExp exp))
         -- local declarations with let ... in ...
    If l exp1 exp2 exp3 {- (Exp l) (Exp l) (Exp l) -}
      -> ("If:" ++ (getDetailsExp exp1) ++ " " ++ (getDetailsExp exp2) ++ " " ++ (getDetailsExp exp3))
         -- if exp then exp else exp
    Case l exp alt {- (Exp l) [Alt l] -}
      -> ("Case:" ++ (getDetailsExp exp) ++ " " ++ ("Alt"))
         -- case exp of alts
    Do l stmts {- [Stmt l] -}
      -> ("Do:")
         -- do-expression: the last statement in the list should be an expression.
    MDo l stmts {- [Stmt l] -}
      -> ("MDo:")
         -- mdo-expression
    Tuple l exps {- [Exp l] -}
      -> ("Tuple:" ++ (concatMap getDetailsExp exps))
         -- tuple expression
    TupleSection l maybeExps {- [Maybe (Exp l)] -}
      -> ("TupleSection:")
         -- tuple section expression, e.g. (,,3)
    List l exps {- [Exp l] -}
      -> ("List:" ++ (concatMap getDetailsExp exps))
         -- list expression
    Paren l exp {- (Exp l) -}
      -> ("Paren:" ++ (getDetailsExp exp))
         -- parenthesised expression
    LeftSection l exp op {- (Exp l) (QOp l) -}
      -> ("LeftSection:" ++ (getDetailsExp exp) ++ " " ++ (show op))
         -- left section (exp qop)
    RightSection l op exp {- (QOp l) (Exp l) -}
      -> ("RightSection:" ++ (show op) ++ " " ++ (getDetailsExp exp))
         -- right section (qop exp)
    RecConstr l name fups {- (QName l) [FieldUpdate l] -}
      -> ("RecConstr:" ++ (getDetailsQName name) ++ " " ++ ("fups"))
         -- record construction expression
    RecUpdate l exp fups {- (Exp l) [FieldUpdate l] -}
      -> ("RecUpdate:" ++ (getDetailsExp exp) ++ " " ++ ("fups"))
         -- record update expression
    EnumFrom l exp {- (Exp l) -}
      -> ("EnumFrom:" ++ (getDetailsExp exp))
         -- unbounded arithmetic sequence, incrementing by 1: [from ..]
    EnumFromTo l exp1 exp2 {- (Exp l) (Exp l) -}
      -> ("EnumFromTo:" ++ (getDetailsExp exp1) ++ " " ++ (getDetailsExp exp2))
         -- bounded arithmetic sequence, incrementing by 1 [from .. to]
    EnumFromThen l exp1 exp2 {- (Exp l) (Exp l) -}
      -> ("EnumFromThen:" ++ (getDetailsExp exp1) ++ " " ++ (getDetailsExp exp2))
         -- unbounded arithmetic sequence, with first two elements given [from, then ..]
    EnumFromThenTo l exp1 exp2 exp3 {- (Exp l) (Exp l) (Exp l) -}
      -> ("EnumFromThenTo:" ++ (getDetailsExp exp1) ++ " " ++ (getDetailsExp exp2) ++
          " " ++ (getDetailsExp exp3))
         -- bounded arithmetic sequence, with first two elements given [from, then .. to]
    ListComp l exp quals {- (Exp l) [QualStmt l] -}
      -> ("ListComp:")
         -- ordinary list comprehension
    ParComp l exp qualss {- (Exp l) [[QualStmt l]] -}
      -> ("ParComp:")
         -- parallel list comprehension
    ExpTypeSig l exp typ {- (Exp l) (Type l) -}
      -> ("ExpTypeSig:")
         -- expression with explicit type signature
    VarQuote l name {- (QName l) -}
      -> ("VarQuote:" ++ (getDetailsQName name))
         -- 'x for template haskell reifying of expressions
         -- terminal
    TypQuote l name {- (QName l) -}
      -> ("TypQuote:" ++ (getDetailsQName name))
         -- ''T for template haskell reifying of types
         -- terminal
    BracketExp l b {- (Bracket l) -}
      -> ("BracketExp:")
         -- template haskell bracket expression
    SpliceExp l splice {- (Splice l) -}
      -> ("SpliceExp:")
         -- template haskell splice expression
    QuasiQuote l s1 s2 {- String String -}
      -> ("QuasiQuote:" ++ s1 ++ " " ++ s2)
         -- quasi-quotaion: [$name| string |]
    XTag l xn xas maybeExp exps {- (XName l) [XAttr l] (Maybe (Exp l)) [Exp l] -}
      -> ("XTag:")
         -- xml element, with attributes and children
    XETag l xn xas maybeExp {- (XName l) [XAttr l] (Maybe (Exp l)) -}
      -> ("XETag:")
         -- empty xml element, with attributes
    XPcdata l s {- String -}
      -> ("XPcdata:" ++ s)
         -- PCDATA child element
    XExpTag l exp {- (Exp l) -}
      -> ("XExpTag:")
         -- escaped haskell expression inside xml
    XChildTag l exps {- [Exp l] -}
      -> ("XChildTag:")
         -- children of an xml element
    CorePragma l s exp {- String (Exp l) -}
      -> ("CorePragma")
         -- CORE pragma
    SCCPragma l s exp {- String (Exp l) -}
      -> ("SCCPragma:")
         -- SCC pragma
    GenPragma l s a b exp {- String (Int, Int) (Int, Int) (Exp l) -}
      -> ("GenPragma:")
         -- GENERATED pragma
    Proc l pat exp {- (Pat l) (Exp l) -}
      -> ("Proc:")
         -- arrows proc: proc pat -> exp
    LeftArrApp l exp1 exp2 {- (Exp l) (Exp l) -}
      -> ("LeftArrApp:")
         -- arrow application (from left): exp -< exp
    RightArrApp l exp1 exp2 {- (Exp l) (Exp l) -}
      -> ("RightArrApp:")
         -- arrow application (from right): exp >- exp
    LeftArrHighApp l exp1 exp2 {- (Exp l) (Exp l) -}
      -> ("LeftArrHighApp:")
         -- higher-order arrow application (from left): exp -<< exp
    RightArrHighApp l exp1 exp2 {- (Exp l) (Exp l) -}
      -> ("RightArrHighApp:")
         -- higher-order arrow application (from right): exp >>- exp

-- ---------------------------------------------------------------------

-- | Get (a) list of top level items
--       (b) for each, a list of the next level down items
getModuleDetails :: FilePath -> IO (String, [[String]])
getModuleDetails filePath = do
  (maybeParse,_notes) <- runCmdVt (BW.getAST filePath)
  case maybeParse of
    (Just x) ->  do
      let (Module _ll modulehead _pragmas _imports decls,_comments) = (fromParseResult x)
          modulename = getModuleNameFromModuleHead filePath modulehead
          declarations = map getDeclDetails decls
      return (modulename,declarations)
    Nothing       -> return (("parse fail:" ++ filePath),[])

-- ---------------------------------------------------------------------

getDetailGraphBasis = do
  files <- getFiles
  m1 <- mapM getModuleDetails files
  return m1

-- ---------------------------------------------------------------------


-- EOF
