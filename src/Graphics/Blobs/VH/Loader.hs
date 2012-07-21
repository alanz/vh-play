{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
module Graphics.Blobs.VH.Loader
       (
         getPage
       , getFiles
       , getAllPages
       , getModuleGraphBasis
       ) where


import Control.Monad.State
import Data.Aeson
import Data.List (foldl')
import Data.Version (showVersion)
import Language.Haskell.BuildWrapper.Base hiding (tempFolder,cabalPath, cabalFile, cabalFlags,verbosity)
import Language.Haskell.BuildWrapper.Cabal
import Language.Haskell.BuildWrapper.GHC
import Language.Haskell.Exts.Annotated.Syntax
import Language.Haskell.Exts.Parser
import Language.Haskell.Exts.SrcLoc
import System.Console.CmdArgs hiding (Verbosity(..),verbosity)
import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Lazy.Char8 as BSC
import qualified Language.Haskell.BuildWrapper.API as BW

type CabalFile = FilePath
type CabalPath = FilePath
type TempFolder = FilePath

-- ---------------------------------------------------------------------

data VhName = VhQName String String -- ^ Qualified name, module and name
            | VhName String         -- ^ unqualified name, name only
              deriving (Show)
-- ---------------------------------------------------------------------

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

getDeclDetails :: Decl SrcSpanInfo -> ([VhName],String)
getDeclDetails decl =
 case decl of
   TypeDecl    _l head typ {-  (DeclHead l) (Type l) -}
     -> ([],"TypeDecl") -- A type declaration
   TypeFamDecl _l head maybeKind {- (DeclHead l) (Maybe (Kind l)) -}
     -> ([],"TypeFamDecl") -- A type family declaration
   DataDecl _l don maybeContext head qual maybeDeriving {- (DataOrNew l) (Maybe (Context l)) (DeclHead l) [QualConDecl l] (Maybe (Deriving l)) -}
     -> ([],"DataDecl") -- A data OR newtype declaration
   GDataDecl _l don maybeContext head maybeKind gadt maybeDeriving {- (DataOrNew l) (Maybe (Context l)) (DeclHead l) (Maybe (Kind l)) [GadtDecl l] (Maybe (Deriving l)) -}
     -> ([],"GDataDecl") -- A data OR newtype declaration, GADT style
   DataFamDecl _l maybeContext head maybeKind {- (Maybe (Context l)) (DeclHead l) (Maybe (Kind l)) -}
     -> ([],"DataFamDecl") -- A data family declaration
   TypeInsDecl _l typ1 typ2 {-  (Type l) (Type l) -}
     -> ([],"TypeInsDecl") -- A type family instance declaration
   DataInsDecl _l don typ qual maybeDeriving {- (DataOrNew l) (Type l) [QualConDecl l] (Maybe (Deriving l)) -}
     -> ([],"DataInsDecl") -- A data family instance declaration
   GDataInsDecl _l don typ maybeKind gadt maybeDeriving {- (DataOrNew l) (Type l) (Maybe (Kind l)) [GadtDecl l] (Maybe (Deriving l)) -}
     -> ([],"GDataInsDecl") -- A data family instance declaration, GADT style
   ClassDecl _l maybeContext head fundep maybeClass {- (Maybe (Context l)) (DeclHead l) [FunDep l] (Maybe [ClassDecl l]) -}
     -> ([],"ClassDecl") -- A declaration of a type class
   InstDecl _l maybeContext head maybeInst {- (Maybe (Context l)) (InstHead l) (Maybe [InstDecl l]) -}
     -> ([],"InstDecl") -- An declaration of a type class instance
   DerivDecl _l maybeContext inst {- (Maybe (Context l)) (InstHead l) -}
     -> ([],"DerivDecl") -- A standalone deriving declaration
   InfixDecl _l assoc maybeInt op {- (Assoc l) (Maybe Int) [Op l] -}
     -> ([],"InfixDecl") -- A declaration of operator fixity
   DefaultDecl _l typ {- [Type l] -}
     -> ([],"DefaultDecl") -- A declaration of default types
   SpliceDecl _l exp {- (Exp l) -}
     -> ([],"SpliceDecl") -- A Template Haskell splicing declaration
   TypeSig _l name typ {- [Name l] (Type l) -}
     -> ([],"TypeSig") -- A type signature declaration
   FunBind _l matches {- [Match l] -}
     -> let
       (xs,s) = getListDetails getMatchDetails matches
       in
        (xs,"(FunBind:" ++ s ++  ")") -- A set of function binding clauses
   PatBind _l pat maybeTyp rhs maybeBinds {- (Pat l) (Maybe (Type l)) (Rhs l) (Maybe (Binds l)) -}
     -> let
       (xps,sp) = getPatDetails pat
       (xrs,sr) = getDetailsRhs rhs
       in
       (xps++xrs,"(PatBind:" ++ sp ++ " "  ++ (getMaybeTypeDetails maybeTyp) ++ " " ++ sr ++ " " ++ (getMaybeBindsDetails maybeBinds) ++ ")") -- A pattern binding
   ForImp _l callconv maybeSafety maybeString name typ {- (CallConv l) (Maybe (Safety l)) (Maybe String) (Name l) (Type l) -}
     -> ([],"ForImp") -- A foreign import declaration
   ForExp _l callconv maybeString name typ {- (CallConv l) (Maybe String) (Name l) (Type l) -}
     -> ([],"ForExp") -- A foreign export declaration
   RulePragmaDecl _l rules {- [Rule l] -}
     -> ([],"RulePragmaDecl") -- A RULES pragma
   DeprPragmaDecl _l pragmas {- [([Name l], String)] -}
     -> ([],"DeprPragmaDecl") -- A DEPRECATED pragma
   WarnPragmaDecl _l pragmas {- [([Name l], String)] -}
     -> ([],"WarnPragmaDecl") -- A WARNING pragma
   InlineSig _l flag maybeActivation name {- Bool (Maybe (Activation l)) (QName l) -}
     -> ([],"InlineSig") -- An INLINE pragma
   InlineConlikeSig _l maybeActivation name {- (Maybe (Activation l)) (QName l) -}
     -> ([],"InlineConLikeSig") -- An INLINE CONLIKE pragma
   SpecSig _l name typ {- (QName l) [Type l] -}
     -> ([],"SpecSig") -- A SPECIALISE pragma
   SpecInlineSig _l flag maybeActivation name typ {- Bool (Maybe (Activation l)) (QName l) [Type l] -}
     -> ([],"SpecInlineSig") -- A SPECIALISE INLINE pragma
   InstSig _l maybeContext head {- (Maybe (Context l)) (InstHead l) -}
     -> ([],"IntSig") -- A SPECIALISE instance pragma
   AnnPragma _l annot {- (Annotation l) -}
     -> ([],"AnnPragma")

-- ---------------------------------------------------------------------

getMaybeTypeDetails :: Maybe (Type l) -> String
getMaybeTypeDetails Nothing = "Nothing"
getMaybeTypeDetails (Just typ) = "(Just type...)"

-- ---------------------------------------------------------------------

getPatDetails :: Pat SrcSpanInfo -> ([VhName],String)
getPatDetails pat =
  case pat of
    PVar _l name {- (Name l) -}
      -> let
        (xs,s) = getDetailsName name
        in
         (xs,"(PVar:" ++ s ++ ")")
         -- variable
    PLit _l lit {- (Literal l) -}
      -> ([],"(PLit:" ++ "Literal.." ++ ")")
         -- literal constant
    PNeg _l p {- (Pat l) -}
      -> let
        (xs,s) = getPatDetails p
        in
         (xs,"(PNeg:" ++ s ++ ")")
         -- negated pattern
    PNPlusK _l name i {- (Name l) Integer -}
      -> let
        (xs,s) = getDetailsName name
        in
         (xs,"(PNPlusK:" ++ s ++ "," ++ (show i) ++ ")")
         -- n+k pattern
    PInfixApp _l lp qn rp {- (Pat l) (QName l) (Pat l) -}
      -> let
        (xsl,sl) = getPatDetails lp
        (xsq,sq) = getQNameDetails qn
        (xsr,sr) = getPatDetails rp
        in
         (xsl++xsq++xsr,"(PInfixApp:" ++ sl ++ "," ++ sq ++ "," ++ sr ++ ")")
         -- pattern with an infix data constructor
    PApp _l qn pats {- (QName l) [Pat l] -}
      -> let
        (xqs,sq) = getQNameDetails qn
        (xps,sp) = getListDetails getPatDetails pats
        in
         (xqs++xps, "(PApp:" ++ sq ++ "," ++ sp ++ ")")
         -- data constructor and argument patterns
    PTuple _l pats {- [Pat l] -}
      -> let
        (xs,s) = getListDetails getPatDetails pats
        in
         (xs,"(PTuple:" ++ s ++ ")")
         -- tuple pattern
    PList _l pats {- [Pat l] -}
      -> let
        (xs,s) = getListDetails getPatDetails pats
        in
         (xs,"(PList:" ++ s ++ ")")
         -- list pattern
    PParen _l p {- (Pat l) -}
      -> let
        (xs,s) = getPatDetails p
        in
        (xs,"(PParen:" ++ s ++ ")")
         -- parenthesized pattern
    PRec _l name field {- (QName l) [PatField l] -}
      -> let
        (xs,s) = getDetailsQName name
        in
         (xs,"(PRec:" ++ s ++ "," ++ (getPatFieldDetails field) ++ ")")
         -- labelled pattern, record style
    PAsPat _l name p {- (Name l) (Pat l) -}
      -> let
        (xns,sn) = getDetailsName name
        (xps,sp) = getPatDetails p
        in
         (xns++xps,"(PAsPat:" ++ sn ++ "," ++ sp ++ ")")
         -- @-pattern
    PWildCard _l
      -> ([],"(PWildCard)")
         -- wildcard pattern: _
    PIrrPat _l p {- (Pat l) -}
      -> let
        (xs,s) = getPatDetails p
        in
         (xs,"PIrrPat:" ++ s ++ ")")
         -- irrefutable pattern: ~pat
    PatTypeSig _l p t {- (Pat l) (Type l) -}
      -> let
        (xs,s) = getPatDetails p
        in
         (xs,"PatTypeSig:" ++ s ++ "," ++ (getTypeDetails t) ++ ")")
         -- pattern with type signature
    PViewPat _l exp p {- (Exp l) (Pat l) -}
      -> let
        (xes,se) = getDetailsExp exp
        (xps,sp) = getPatDetails p
        in
         (xes++xps,"(PViewPat:" ++ se ++ "," ++ sp ++ ")")
         -- view patterns of the form (exp -> pat)
    PRPat _l rpats {- [RPat l] -}
      -> ([],"(PRPat:" ++ ("...") ++ ")")
         -- regular list pattern
    PXTag _l _xn _xas _mp _pats {- (XName l) [PXAttr l] (Maybe (Pat l)) [Pat l] -}
      -> ([],"(PXTag:...)")
         -- XML element pattern
    PXETag _l _xn _xas _mp {-  (XName l) [PXAttr l] (Maybe (Pat l)) -}
      -> ([],"(PXETag:...)")
         -- XML singleton element pattern
    PXPcdata _l s {- String -}
      -> ([],"(PXPcdata:" ++ s ++ ")")
         -- XML PCDATA pattern
    PXPatTag _l _p {- (Pat l) -}
      -> ([],"(PXPatTag:..)")
         -- XML embedded pattern
    PXRPats _l _pats {- l [RPat l] -}
      -> ([],"(PXRPats:...)")
         -- XML regular list pattern
    PExplTypeArg _l _qn _t {- (QName l) (Type l) -}
      -> ([],"(PExplTypeArg:...)")
         -- Explicit generics style type argument e.g. f {| Int |} x = ...
    PQuasiQuote _l s1 s2 {- String String -}
      -> ([],"(PQuasiQuote:" ++ s1 ++ "," ++ s2 ++ ")")
         -- quasi quote pattern: [$name| string |]
    PBangPat _l p {- (Pat l) -}
      -> let
        (xs,s) = getPatDetails p
        in
         (xs,"(PBangPat:" ++ s ++ ")")
         -- strict (bang) pattern: f !x = ...

-- ---------------------------------------------------------------------
{-
getPatListDetails pats =
  foldl'
  (\(xsa,sa) (xsn,sn)-> (xsa++xsn,sa++sn))
  ([],"")
  $ map getPatDetails pats
-}

getListDetails f xs =
  foldl'
  (\(xsa,sa) (xsn,sn)-> (xsa++xsn,sa++sn))
  ([],"")
  $ map f xs

-- ---------------------------------------------------------------------

getTypeDetails typ = "(Type:...)"

-- ---------------------------------------------------------------------

getPatFieldDetails typ = "(Type:...)"

-- ---------------------------------------------------------------------

getDetailsRhs :: Rhs SrcSpanInfo -> ([VhName],String)
getDetailsRhs rhs =
  case rhs of
    UnGuardedRhs _l exp {- (Exp l) -}
      -> let
        (xs,s) = getDetailsExp exp
        in
         (xs,"UnguardedRhs:" ++ s)
         -- unguarded right hand side (exp)
    GuardedRhss _l grhss {- [GuardedRhs l] -}
      -> ([],"(GuardedRhss:...)")
         -- guarded right hand side (gdrhs)

-- ---------------------------------------------------------------------

getMatchDetails match =
  case match of
    Match _l name pats rhs maybeBinds {- (Name l) [Pat l] (Rhs l) (Maybe (Binds l)) -}
      -> let
        (xns,sn) = getDetailsName name
        (xps,sp) = getListDetails getPatDetails pats
        (xrs,sr) = getDetailsRhs rhs
        in
         (xns++xps++xrs,"Match:" ++ sn ++ " " ++ sp
          ++ " " ++ sr ++ " " ++ (getMaybeBindsDetails maybeBinds))
         -- A clause defined with prefix notation, i.e. the function
         -- name followed by its argument patterns, the right-hand
         -- side and an optional where clause.

    InfixMatch _l pat name pats rhs maybeBinds {- (Pat l) (Name l) [Pat l] (Rhs l) (Maybe (Binds l)) -}
      -> let
        (xns,sn) = getDetailsName name
        (xrs,sr) = getDetailsRhs rhs
        in
         (xns++xrs,"InfixMatch:" ++ sn ++ " " ++ sr ++ ")")
         -- A clause defined with infix notation, i.e. first its
         -- first argument pattern, then the function name, then its
         -- following argument(s), the right-hand side and an
         -- optional where clause. Note that there can be more than
         -- two arguments to a function declared infix, hence the
         -- list of pattern arguments.

-- ---------------------------------------------------------------------

getMaybeBindsDetails Nothing = "Nothing"
getMaybeBindsDetails (Just binds) = getBindsDetails binds

-- ---------------------------------------------------------------------

getDetailsName :: Name SrcSpanInfo -> ([VhName],String)
getDetailsName name =
  case name of
    Ident _l s {- String -}
      -> ([VhName ("Ident:"++s)],"(Ident:" ++ s ++ ")")
         -- varid or conid.
    Symbol _l s {- String -}
      -> ([VhName ("Symbol:"++s)],"(Symbol:" ++ s ++ ")")
         -- varsym or consym


-- ---------------------------------------------------------------------

getDetailsQName :: QName SrcSpanInfo -> ([VhName],String)
getDetailsQName qname =
  case qname of
    Qual _l (ModuleName _l2 mname) name {- (ModuleName l) (Name l) -}
      -> let
        ([VhName n],s) = getDetailsName name
        in
         ([VhQName mname n],"Qual:" ++ mname ++ "," ++ s ++ ")")
         -- name qualified with a module name
    UnQual _l name {- (Name l) -}
      -> let
        (xs,s) = getDetailsName name
        in
         (xs,"(UnQual:" ++ s ++ ")")
         -- unqualified local name
    Special _l sp {- (SpecialCon l) -}
      -> ([],"Special:")
         -- built-in constructor with special syntax

-- ---------------------------------------------------------------------

-- getDetailsModuleName (ModuleName _l s) = ("(ModuleName:" ++ s ++ ")")

-- ---------------------------------------------------------------------

getDetailsExp exp =
  case exp of
    Var _l name {- (QName l) -}
      -> let
        (xs,s) = getDetailsQName name
        in
         (xs,"(Var:" ++ s ++ ")") -- variable
         -- terminal
    IPVar _l ipname {- (IPName l) -}
      -> ([VhName (show ipname)],"IPVar:" ++ (show ipname)) -- implicit parameter variable
         -- terminal
    Con _l name {- (QName l) -}
      -> let
        (xs,s) = getDetailsQName name
        in
         (xs,"Con:" ++ s) -- data constructor
         -- terminal
    Lit _l lit {- (Literal l) -}
      -> ([],"Lit:)") -- literal constant
         -- terminal
    InfixApp _l exp1 op exp2 {- (Exp l) (QOp l) (Exp l) -}
      -> let
        (x1s,s1) = getDetailsExp exp1
        (xos,so) = getDetailsQOp op
        (x2s,s2) = getDetailsExp exp2
        in
        (x1s++xos++x2s,"InfixApp:" ++ s1 ++ " " ++ so ++ " " ++ s2)
         -- infix application
    App _l exp1 exp2 {- (Exp l) (Exp l) -}
      -> let
        (x1s,s1) = getDetailsExp exp1
        (x2s,s2) = getDetailsExp exp2
        in
         (x1s++x2s,"App:" ++ s1 ++ " " ++ s2)
         -- ordinary application
    NegApp _l exp {- (Exp l) -}
      -> let
        (xs,s) = getDetailsExp exp
        in
         (xs,"NegApp:" ++ s)
         -- negation expression -exp (unary minus)
    Lambda _l pats exp {- [Pat l] (Exp l) -}
      -> ([],"Lambda")
         -- lambda expression
    Let _l binds exp {- (Binds l) (Exp l) -}
      -> let
        (xs,s) = getDetailsExp exp
        in
         (xs,"Let:" ++ (show binds) ++ " " ++ s)
         -- local declarations with let ... in ...
    If _l exp1 exp2 exp3 {- (Exp l) (Exp l) (Exp l) -}
      -> let
        (x1s,s1) = getDetailsExp exp1
        (x2s,s2) = getDetailsExp exp2
        (x3s,s3) = getDetailsExp exp3
        in
         (x1s++x2s++x3s,"If:" ++ s1 ++ " " ++ s2 ++ " " ++ s3)
         -- if exp then exp else exp
    Case _l exp alt {- (Exp l) [Alt l] -}
      -> let
        (xs,s) = getDetailsExp exp
        in
         (xs,"Case:" ++ s ++ " " ++ ("Alt"))
         -- case exp of alts
    Do _l stmts {- [Stmt l] -}
      -> let
        (xs,s) = getListDetails getStmtDetails stmts
        in
         (xs,"(Do:" ++ s ++ ")")
         -- do-expression: the last statement in the list should be an expression.
    MDo _l stmts {- [Stmt l] -}
      -> ([],"MDo:")
         -- mdo-expression
    Tuple _l exps {- [Exp l] -}
      -> let
        (xs,s) = getListDetails getDetailsExp exps
        in
         (xs,"Tuple:" ++ s)
         -- tuple expression
    TupleSection _l maybeExps {- [Maybe (Exp l)] -}
      -> ([],"TupleSection:")
         -- tuple section expression, e.g. (,,3)
    List _l exps {- [Exp l] -}
      -> let
        (xs,s) = getListDetails getDetailsExp exps
        in
         (xs,"List:" ++ s)
         -- list expression
    Paren _l exp {- (Exp l) -}
      -> let
        (xs,s) = getDetailsExp exp
        in
         (xs,"Paren:" ++ s)
         -- parenthesised expression
    LeftSection _l exp op {- (Exp l) (QOp l) -}
      -> let
        (xs,s) = getDetailsExp exp
        in
         (xs,"LeftSection:" ++ s ++ " " ++ (show op))
         -- left section (exp qop)
    RightSection _l op exp {- (QOp l) (Exp l) -}
      -> let
        (xs,s) = getDetailsExp exp
        in
         (xs,"RightSection:" ++ (show op) ++ " " ++ s)
         -- right section (qop exp)
    RecConstr _l name fups {- (QName l) [FieldUpdate l] -}
      -> let
        (xs,s) = getDetailsQName name
        in
         (xs,"RecConstr:" ++ s ++ " " ++ ("fups"))
         -- record construction expression
    RecUpdate _l exp fups {- (Exp l) [FieldUpdate l] -}
      -> let
        (xs,s) = getDetailsExp exp
        in
         (xs,"RecUpdate:" ++ s ++ " " ++ ("fups"))
         -- record update expression
    EnumFrom _l exp {- (Exp l) -}
      -> let
        (xs,s) = getDetailsExp exp
        in
         (xs,"EnumFrom:" ++ s)
         -- unbounded arithmetic sequence, incrementing by 1: [from ..]
    EnumFromTo _l exp1 exp2 {- (Exp l) (Exp l) -}
      -> let
        (x1s,s1) = getDetailsExp exp1
        (x2s,s2) = getDetailsExp exp2
        in
         (x1s++x2s,"EnumFromTo:" ++ s1 ++ " " ++ s2)
         -- bounded arithmetic sequence, incrementing by 1 [from .. to]
    EnumFromThen _l exp1 exp2 {- (Exp l) (Exp l) -}
      -> let
        (x1s,s1) = getDetailsExp exp1
        (x2s,s2) = getDetailsExp exp2
        in
         (x1s++x2s,"EnumFromThen:" ++s1 ++ " " ++ s2)
         -- unbounded arithmetic sequence, with first two elements given [from, then ..]
    EnumFromThenTo _l exp1 exp2 exp3 {- (Exp l) (Exp l) (Exp l) -}
      -> let
        (x1s,s1) = getDetailsExp exp1
        (x2s,s2) = getDetailsExp exp2
        (x3s,s3) = getDetailsExp exp3
        in
         (x1s++x2s++x3s,"EnumFromThenTo:" ++ s1 ++ " " ++ s2 ++ " " ++ s3)
         -- bounded arithmetic sequence, with first two elements given [from, then .. to]
    ListComp _l exp quals {- (Exp l) [QualStmt l] -}
      -> ([],"ListComp:")
         -- ordinary list comprehension
    ParComp _l exp qualss {- (Exp l) [[QualStmt l]] -}
      -> ([],"ParComp:")
         -- parallel list comprehension
    ExpTypeSig _l exp typ {- (Exp l) (Type l) -}
      -> ([],"ExpTypeSig:")
         -- expression with explicit type signature
    VarQuote _l name {- (QName l) -}
      -> let
        (xs,s) = getDetailsQName name
        in
         (xs,"VarQuote:" ++ s)
         -- 'x for template haskell reifying of expressions
         -- terminal
    TypQuote _l name {- (QName l) -}
      -> let
        (xs,s) = getDetailsQName name
        in
         (xs,"TypQuote:" ++ s)
         -- ''T for template haskell reifying of types
         -- terminal
    BracketExp _l b {- (Bracket l) -}
      -> ([],"BracketExp:")
         -- template haskell bracket expression
    SpliceExp _l splice {- (Splice l) -}
      -> ([],"SpliceExp:")
         -- template haskell splice expression
    QuasiQuote _l s1 s2 {- String String -}
      -> ([],"QuasiQuote:" ++ s1 ++ " " ++ s2)
         -- quasi-quotaion: [$name| string |]
    XTag _l xn xas maybeExp exps {- (XName l) [XAttr l] (Maybe (Exp l)) [Exp l] -}
      -> ([],"XTag:")
         -- xml element, with attributes and children
    XETag _l xn xas maybeExp {- (XName l) [XAttr l] (Maybe (Exp l)) -}
      -> ([],"XETag:")
         -- empty xml element, with attributes
    XPcdata _l s {- String -}
      -> ([],"XPcdata:" ++ s)
         -- PCDATA child element
    XExpTag _l exp {- (Exp l) -}
      -> ([],"XExpTag:")
         -- escaped haskell expression inside xml
    XChildTag _l exps {- [Exp l] -}
      -> ([],"XChildTag:")
         -- children of an xml element
    CorePragma _l s exp {- String (Exp l) -}
      -> ([],"CorePragma")
         -- CORE pragma
    SCCPragma _l s exp {- String (Exp l) -}
      -> ([],"SCCPragma:")
         -- SCC pragma
    GenPragma _l s a b exp {- String (Int, Int) (Int, Int) (Exp l) -}
      -> ([],"GenPragma:")
         -- GENERATED pragma
    Proc _l pat exp {- (Pat l) (Exp l) -}
      -> ([],"Proc:")
         -- arrows proc: proc pat -> exp
    LeftArrApp _l exp1 exp2 {- (Exp l) (Exp l) -}
      -> ([],"LeftArrApp:")
         -- arrow application (from left): exp -< exp
    RightArrApp _l exp1 exp2 {- (Exp l) (Exp l) -}
      -> ([],"RightArrApp:")
         -- arrow application (from right): exp >- exp
    LeftArrHighApp _l exp1 exp2 {- (Exp l) (Exp l) -}
      -> ([],"LeftArrHighApp:")
         -- higher-order arrow application (from left): exp -<< exp
    RightArrHighApp _l exp1 exp2 {- (Exp l) (Exp l) -}
      -> ([],"RightArrHighApp:")
         -- higher-order arrow application (from right): exp >>- exp

-- ---------------------------------------------------------------------

getStmtDetails :: Stmt SrcSpanInfo -> ([VhName],String)
getStmtDetails stmt =
  case stmt of
    Generator _l pat exp {- (Pat l) (Exp l) -}
      -> let
        (xps,sp) = getPatDetails pat -- TODO: suspect xps is local only
        (xes,se) = getDetailsExp exp
        in
         (xps++xes,"(Generator:" ++ sp ++ "," ++ se ++ ")")
         -- a generator: pat <- exp
    Qualifier _l exp {- (Exp l) -}
      -> let
        (xs,s) = getDetailsExp exp
        in
         (xs,"(Qualifier:" ++ s ++ ")")
         -- an exp by itself: in a do-expression, an action whose
         -- result is discarded; in a list comprehension and pattern
         -- guard, a guard expression
    LetStmt _l bs {- (Binds l) -}
      -> ([],"(LetStmst:" ++ (getBindsDetails bs) ++ ")")
         -- local bindings
    RecStmt _l stmts {- [Stmt l] -}
      -> let
        (xs,s) = getListDetails getStmtDetails stmts
        in
         (xs,"(RecStmt:" ++ s ++ ")")
         -- a recursive binding group for arrows

-- ---------------------------------------------------------------------

getBindsDetails binds =
  case binds of
    BDecls _l decls {- [Decl l]	-}
      -> ("(BDecls:" ++ (concat $ map (\(_,s) -> s) $ map getDeclDetails decls) ++ ")")
         -- An ordinary binding group
    IPBinds _l ipbs {- [IPBind l] -}
      -> ("(IPBinds:" ++ "..." ++ ")")
         -- A binding group for implicit parameters

-- ---------------------------------------------------------------------

getDetailsQOp op =
  case op of
    QVarOp _l name {- (QName l)	-}
      -> let
        (xs,s) = getQNameDetails name
        in
         (xs,"QVarOp:"++ s)
         -- variable operator (qvarop)
    QConOp _l name {- (QName l)	-}
      -> let
        (xs,s) = getQNameDetails name
        in
         (xs,"QConOp:" ++ s)
         -- constructor operator (qconop)

getQNameDetails :: QName SrcSpanInfo -> ([VhName],String)
getQNameDetails name =
  case name of
    Qual _l (ModuleName _l2 modname) n {- (ModuleName l) (Name l) -}
      -> let
        ([VhName ns],s) = getDetailsName n
        in
         ([VhQName modname ns],"(Qual:" ++ modname ++ ":" ++ s ++ ")")
         -- name qualified with a module name
    UnQual _l n {- (Name l) -}
      -> let
        (xs,s) = getDetailsName n
        in
         (xs,"(UnQual:" ++ s ++ ")")
         -- unqualified local name
    Special _l s {- (SpecialCon l) -}
      -> ([],"(Special:)")
         -- built-in constructor with special syntax

-- ---------------------------------------------------------------------

-- | Get (a) list of top level items
--       (b) for each, a list of the next level down items
-- getModuleDetails :: FilePath -> IO (String, [[String]])
getModuleDetails :: FilePath -> IO (String, [([VhName], String)])
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
