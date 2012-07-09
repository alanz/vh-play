--A.hs
--invoke: ghci -package ghc A.hs

import GHC
import Outputable
import Bag
import SrcLoc
import GHC.Paths ( libdir )
--GHC.Paths is available via cabal install ghc-paths

import DynFlags
import Name

targetFile = "./src/B.hs"
-- targetFile = "./src/A.hs"

main = do
   res <- example
   putStrLn $ showSDoc ( ppr res )

example :: IO TypecheckedSource
example =
    defaultErrorHandler defaultLogAction $ do
      runGhc (Just libdir) $ do
        dflags <- getSessionDynFlags
        let dflags' = foldl xopt_set dflags
                            [Opt_Cpp, Opt_ImplicitPrelude, Opt_MagicHash]
        setSessionDynFlags dflags'
        target <- guessTarget targetFile Nothing
        setTargets [target]
        load LoadAllTargets
        modSum <- getModSummary $ mkModuleName "B"
        p <- parseModule modSum
        t <- typecheckModule p
        d <- desugarModule t
        l <- loadModule d
        n <- getNamesInScope
        c <- return $ coreModule d

        g <- getModuleGraph
        -- mapM showModule g
        -- return $ (parsedSource d,"/n-----/n",  typecheckedSource d, "/n-=-=-=-=-=-=-/n", modInfoTyThings $ moduleInfo t)
        -- return $ (parsedSource d,"/n-----/n",  typecheckedSource d, "/n-=-=-=-=-=-=-/n")
        return $ (typecheckedSource d)


mymain :: IO ()
mymain = do
  res <- example
  putStrLn $ showLhsBinds res
  -- let lhsbinds = bagToList res
  -- putStrLn $ concatMap (\b -> "," ++ (myShow $ unLoc b)) lhsbinds

showLhsBinds res = concatMap (\b -> "," ++ (myShow $ unLoc b)) $ bagToList res


myShow :: (Show a, Show t) => HsBindLR a t -> String
myShow x = case x of
  (FunBind  fid _isInfix matches _coercion _fvs _tickish ) -> "(FunBind:" ++ (show (unLoc fid)) ++ "[" ++ (showMatchGroup matches) ++ "]" ++ ")"
  (PatBind  _a _b _c _d _e   ) -> "PatBind"
  (VarBind  _a _b _c       ) -> "VarBind"
  (AbsBinds tvs evvars exports _evbinds binds) -> "AbsBinds" ++ (sie tvs) ++ (sie evvars) ++ (concatMap showExport exports) {- ++ (sie evbinds) -}  ++ (concatMap (\b -> myShow $ unLoc b) $ bagToList binds)

sie :: [t] -> String
sie []  = "[]"
sie _xs  = "[x]"

showExport :: Show a => ABExport a -> String
showExport (ABE poly mono _wrap _prags) = "(ABE " ++ (show poly) ++ "," ++ (show mono) ++ ")"

-- showBinds b = "foo"

showMatchGroup :: Show t => MatchGroup t -> String
showMatchGroup (MatchGroup ms _ptc) = "(" ++ (concatMap (\m -> showMatch $ unLoc m) ms) ++ ")"

showMatch :: Show t => Match t -> String
showMatch (Match lpats _mt rhs) = "(Match [" ++ concatMap (\l -> showPat $ unLoc l) lpats ++ "] " ++ (showGRHSs rhs) ++ ")"

showPat :: Show a => Pat a -> String
showPat x = case x of
  VarPat id -> "(VarPat " ++ (show id) ++ ")"
  _ -> "Unknown Pat"

showGRHSs :: Show t => GRHSs t -> String
showGRHSs (GRHSs guards lb) = "(GRHSs [" ++ (concatMap (\g -> showGuard $ unLoc g) guards) ++ "] " ++ (showLocalBinds lb) ++ " )"

showGuard :: Show a => GRHS a -> String
showGuard (GRHS lstmts expr) = "(GRHS [" ++ (concatMap (\l -> showStmtLR $ unLoc l) lstmts) ++ "] "++ (showHsExpr $ unLoc expr) ++ ")"

showStmtLR x = case x of
  LastStmt l s {- (LHsExpr idR) (SyntaxExpr idR) -} -> "(LastStmt)"
  BindStmt lp l s1 s2 {- (LPat idL) (LHsExpr idR) (SyntaxExpr idR) (SyntaxExpr idR) -} -> "(BindStmt)"
  ExprStmt e s1 s2 pt {- (LHsExpr idR) (SyntaxExpr idR) (SyntaxExpr idR) PostTcType -} -> "(ExprStmt)"
  LetStmt lb  {- (HsLocalBindsLR idL idR) -} -> "(LetStmt)"
  ParStmt a b c d {- [([LStmt idL], [idR])] (SyntaxExpr idR) (SyntaxExpr idR) (SyntaxExpr idR) -} -> "(ParStmt)"
  TransStmt a b c d e f g h -> "(TransStmt)"
  RecStmt a b c d e f g h i -> "(RecStmt)"


showHsExpr :: (Show a) => HsExpr a -> String
showHsExpr e = case e of
  HsVar vid -> "(HsVar " ++ (show vid) ++ ")"
  HsIPVar _id -> "(HsIPVar)"
  HsOverLit (OverLit v _rb _w _t) -> "(HsOverLit " ++ (showOverLit v)++ ")"
  HsLit _lit -> "(HsLit)"
  HsLam _mg -> "(HsLam)"
  HsApp _e1 _e2 -> "(HsApp)"
  OpApp e1 e2 _fixity e3 -> "(OpApp:" ++ (showHsExpr $ unLoc e1) ++ "," ++ (showHsExpr $ unLoc e2) ++ "," ++ (showHsExpr $ unLoc e3) ++ ")"
  NegApp _e1 _se1 -> "(NegApp)"
  HsPar _e1 -> "(HsPar)"
  SectionL _e1 _e2 -> "(SectionL)"
  SectionR _e1 _e2 -> "(SectionR)"
  ExplicitTuple _tuparg _boxity -> "(ExplicitTuple)"
  HsCase _e1 _mg -> "(HsCase)"
  HsIf _mse _e1 _e2 _e3 -> "(HsIf)"
  HsLet _lb _e1 -> "(HsLet)"
  HsDo n ls t -> "(HsDo"++(showHsStmtContext n) ++ "," ++(concatMap showLStmt ls)++","++(showPostTcType t)++")"
  ExplicitList _t _e1 -> "(ExplicitList)"
  ExplicitPArr _t _e1 -> "(ExplicitPArr)"
  RecordCon _li _te _rb -> "(RecordCon)"
  RecordUpd _e1 _rb _dcs _ts1 _ts2 -> "(RecordUpd)"
  ExprWithTySig _e1 _t -> "(ExprWithTySig)"
  ExprWithTySigOut _e1 _t -> "(ExprWithTySigOut)"
  ArithSeq _te _si -> "(ArithSeqInfo)"
  PArrSeq _te _si -> "(PArrSeq)"
  HsSCC _fs _e1 -> "(HsSCC)"
  HsCoreAnn _fs _e1 -> "(HsCoreAnn)"
  HsBracket _b -> "(HsBracket)"
  HsBracketOut _b _ps -> "(HsBracketOut)"
  HsSpliceE _s -> "(HsSplice)"
  HsQuasiQuoteE _qq -> "(HsQuasiQuote)"
  HsProc _lp _lc -> "(HsProc)"
  HsArrApp _e1 _e2 _t _at _b -> "(HsArrAppType)"
  HsArrForm _e1 _mf _lcs -> "(HsArrForm)"
  HsTick _ti _e1 -> "(HsTick)"
  HsBinTick _i1 _i2 _e1 -> "(HsBinTick)"
  HsTickPragma _a _b -> "(HsTickPragma)"
  EWildPat -> "(EWildPat)"
  EAsPat _lid _e1 -> "(EAsPat)"
  EViewPat _e1 _e2 -> "(EViewPat)"
  ELazyPat _e1 -> "(ELazyPat)"
  HsType _lt -> "(HsType)"
  HsWrap _w e1 -> "(HsWrap:(HsWrapper)," ++ (showHsExpr e1) ++ ")"
  -- _         -> "(unk HsExpr)"

showHsStmtContext sc = case sc of
  ListComp -> "(ListComp)"
  MonadComp -> "(MonadComp)"
  PArrComp -> "(PArrComp)"
  DoExpr -> "(DoExpr)"
  MDoExpr -> "(MDoExpr)"
  ArrowExpr -> "(ArrowExpr)"
  GhciStmt -> "(GhciStmt)"
  PatGuard c {- (HsMatchContext id) -} -> "(PatGuard)"
  ParStmtCtxt c {- (HsStmtContext id) -} -> "(ParStmtCtxt)"
  TransStmtCtxt c {- (HsStmtContext id) -} -> "(TransStmtCtxt)"

showLStmt ls = showStmtLR $ unLoc ls

showPostTcType ptt = "(PostTcType)"

showOverLit :: OverLitVal -> String
showOverLit (HsIntegral i) = show i
showOverLit (HsFractional fl) = show fl
showOverLit (HsIsString fs) = show fs

showLocalBinds :: (Show t) => HsLocalBindsLR t t1 -> String
showLocalBinds x = case x of
  HsValBinds vb -> "(HsValBinds:" ++ (showHsValBindsLR vb) ++ ")"
  HsIPBinds ipb -> "(HsIPBinds)"
  EmptyLocalBinds -> "EmptyLocalBinds"

showHsValBindsLR :: (Show idL) => (HsValBindsLR idL idR) -> String
showHsValBindsLR vb = case vb of
  ValBindsIn b s {- (LHsBindsLR idL idR) [LSig idR] -} -> "(ValBindsIn)"
  ValBindsOut bs ss {- [(RecFlag, LHsBinds idL)] [LSig Name] -} -> "(ValBindsOut:" ++ (concatMap show1 bs) ++ "," ++ (concatMap show2 ss) ++ ")"
  where
    show1 (_recFlag, binds) = "(recFlag," ++ (showLhsBinds binds) ++ ")"

    show2 :: LSig Name -> String
    show2 lsig = showSig $ unLoc lsig

showSig :: Sig Name -> String
showSig sig = case sig of
  TypeSig lns t {- [Located name] (LHsType name) -} -> "(TypeSig [" ++ (concatMap (\n -> getOccString $ unLoc n) lns) ++ "]," ++ (showHsType $ unLoc t) ++ ")"
  GenericSig lns t {- [Located name] (LHsType name) -} -> "(GenericSig)"
  IdSig sid -> "(IdSig " ++ (show sid) ++ ")"
  FixSig f {- (FixitySig name) -} -> "(FixSig)"
  InlineSig ln p {- (Located name) InlinePragma	-} -> "(InlineSig)"
  SpecSig ln t p {- (Located name) (LHsType name) InlinePragma -} -> "(SpecSig)"
  SpecInstSig ln {- (LHsType name) -} -> "(SpecIntSig)"


showHsType :: HsType Name -> String
showHsType t = case t of
  HsForAllTy e ns c t {- HsExplicitFlag [LHsTyVarBndr name] (LHsContext name) (LHsType name) -} -> "(HsForAllTy)"
  HsTyVar name -> "(HsTyVar " ++ (getOccString name) ++ ")"
  HsAppTy lt rt {- (LHsType name) (LHsType name) -} -> "(HsAppTy)"
  HsFunTy lt rt {- (LHsType name) (LHsType name) -} -> "(HsFunTy)"
  HsListTy n {- (LHsType name) -} -> "(HsListTy)"
  HsPArrTy n {- (LHsType name) -} -> "(HsPArrTy)"
  HsTupleTy ts ns {- HsTupleSort [LHsType name] -} -> "(HsTupleTy)"
  HsOpTy lt o rt {- (LHsType name) (LHsTyOp name) (LHsType name) -} -> "(HsOpTy)"
  HsParTy n {- (LHsType name) -} -> "(HsParTy)"
  HsIParamTy i n {- (IPName name) (LHsType name) -} -> "(HsIParamTy)"
  HsEqTy lt rt {- (LHsType name) (LHsType name) -} -> "(HsEqTy)"
  HsKindSig t k {- (LHsType name) (LHsKind name) -} -> "(HsKindSig)"
  HsQuasiQuoteTy qq {- (HsQuasiQuote name) -} -> "(HsQuasiQuote)"
  HsSpliceTy n fv k {- (HsSplice name) FreeVars PostTcKind -} -> "(HsSpliceTy)"
  HsDocTy n ds {- (LHsType name) LHsDocString -} -> "(HsDocTy)"
  HsBangTy b n {- HsBang (LHsType name) -} -> "(HsBangTy)"
  HsRecTy rns {- [ConDeclField name] -} -> "(HsRecTy)"
  HsCoreTy ct {- Type -} -> "(HsCoreTy)"
  HsExplicitListTy pt ns {- PostTcKind [LHsType name] -} -> "(HsExplicitListTy)"
  HsExplicitTupleTy pts ns {- [PostTcKind] [LHsType name] -} -> "(HsExplicitTupleTy)"
  HsWrapTy w n {- HsTyWrapper (HsType name) -} -> "(HsWrapTy)"


-- getBind :: Located e -> e
-- getBind (GenLocated span b) = b
-- getBind x = undefined



{-

        typecheckedSource d :: TypecheckedSource

        type TypecheckedSource = LHsBinds Id
        type Id = Var
        data Var
         Essentially a typed Name, that may also contain some
             additional information about the Var and it's use sites.

        type LHsBinds id = LHsBindsLR id id

        type LHsBindsLR idL idR = Bag (LHsBindLR idL idR)

        bagToList :: Bag a -> [a]

        type LHsBindLR idL idR = Located (HsBindLR idL idR)

data HsBindLR idL idR Source

Constructors
FunBind

FunBind is used for both functions f x = e and variables f = x -> e

Reason 1: Special case for type inference: see tcMonoBinds.

Reason 2: Instance decls can only have FunBinds, which is convenient. If you change this, you'll need to change e.g. rnMethodBinds

But note that the form f :: a->a = ... parses as a pattern binding, just like (f :: a -> a) = ...

fun_id :: Located idL
fun_infix :: Bool

    True => infix declaration
fun_matches :: MatchGroup idR

    The payload
fun_co_fn :: HsWrapper

    Coercion from the type of the MatchGroup to the type of the Id. Example: f :: Int -> forall a. a -> a f x y = y Then the MatchGroup will have type (Int -> a' -> a') (with a free type variable a'). The coercion will take a CoreExpr of this type and convert it to a CoreExpr of type Int -> forall a'. a' -> a' Notice that the coercion captures the free a'.
bind_fvs :: NameSet

    After the renamer, this contains the locally-bound free variables of this defn. See Note [Bind free vars]
fun_tick :: Maybe (Tickish Id)

    Tick to put on the rhs, if any

PatBind

pat_lhs :: LPat idL
pat_rhs :: GRHSs idR
pat_rhs_ty :: PostTcType
bind_fvs :: NameSet

    After the renamer, this contains the locally-bound free variables of this defn. See Note [Bind free vars]
pat_ticks :: (Maybe (Tickish Id), [Maybe (Tickish Id)])

    Tick to put on the rhs, if any, and ticks to put on the bound variables.

VarBind

var_id :: idL
var_rhs :: LHsExpr idR
var_inline :: Bool

AbsBinds

abs_tvs :: [TyVar]
abs_ev_vars :: [EvVar]
abs_exports :: [ABExport idL]
abs_ev_binds :: TcEvBinds
abs_binds :: LHsBinds idL

-}