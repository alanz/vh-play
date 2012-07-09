module GhcNet where

-- | This module extracts a simplified graph of the elements we care
-- about, from a parsed source

import Bag
import DynFlags
import GHC
import GHC.Paths ( libdir )
import Name
import Outputable
import SrcLoc
import RdrName

-- ---------------------------------------------------------------------

targetFile = "./src/B.hs"

-- targetFile = "./src/A.hs"

example :: IO ParsedModule
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
        -- t <- typecheckModule p
        -- d <- desugarModule t
        -- l <- loadModule d
        -- n <- getNamesInScope
        -- c <- return $ coreModule d

        -- g <- getModuleGraph

        -- mapM showModule g
        -- return $ (parsedSource d,"/n-----/n",  typecheckedSource d, "/n-=-=-=-=-=-=-/n", modInfoTyThings $ moduleInfo t)
        -- return $ (parsedSource d,"/n-----/n",  typecheckedSource d, "/n-=-=-=-=-=-=-/n")
        -- return $ (typecheckedSource d)
        return p


mymain :: IO ()
mymain = do
  res <- example
  putStrLn $ show $ getParsedModule res
  -- let lhsbinds = bagToList res
  -- putStrLn $ concatMap (\b -> "," ++ (myShow $ unLoc b)) lhsbinds

-- ---------------------------------------------------------------------
data Node = Node String
          | NodeFunBind String [Node]
          | NodeMatch [Node] (Maybe Node) Node
          | NodeGRHSs [Node] Node
          | NodeGRHS  [Node] Node
          deriving (Show)

getParsedModule :: ParsedModule -> [Node]
getParsedModule pm =
  let
    ps :: Located (HsModule RdrName)
    ps = pm_parsed_source pm
  in
   getHsModule $ unLoc ps

getHsModule :: HsModule RdrName -> [Node]
getHsModule modu =
  let
    -- exports = hsmodExports mod
    -- imports = hsmodImports mod
    decls :: [LHsDecl RdrName]
    decls   = hsmodDecls modu
  in
   map getLHsDecl decls

-- getLHsDecl :: GenLocated l (HsDecl t) -> Node
getLHsDecl :: Located (HsDecl RdrName) -> Node
getLHsDecl decl = getHsDecl $ unLoc decl

getHsDecl :: HsDecl RdrName -> Node
getHsDecl d = case d of
  TyClD cl {- (TyClDecl id) -} -> Node "TyClDecl"
  InstD i {- (InstDecl id) -} -> Node "InstD"
  DerivD d {- (DerivDecl id) -} -> Node "DerivD"
  ValD b {- (HsBind id) -} -> getHsBind b
  SigD s {- (Sig id) -} -> Node "SigD"
  DefD d {- (DefaultDecl id) -} -> Node "DefD"
  ForD f {- (ForeignDecl id) -} -> Node "ForD"
  WarningD w {- (WarnDecl id) -} -> Node "WarningD"
  AnnD a {- (AnnDecl id) -} -> Node "AnnD"
  RuleD r {- (RuleDecl id) -} -> Node "RuleD"
  VectD v {- (VectDecl id) -} -> Node "VectD"
  SpliceD s {- (SpliceDecl id) -} -> Node "SpliceD"
  DocD d {- DocDecl -} -> Node "DocD"
  QuasiQuoteD qq {- (HsQuasiQuote id) -} -> Node "QuasiQuoteD"


getHsBind :: HsBind RdrName -> Node
getHsBind b = case b of
  (FunBind  fid _isInfix matches _coercion _fvs _tickish ) -> NodeFunBind (showRdrName (unLoc fid)) (getMatchGroup matches)
  (PatBind  _a _b _c _d _e   ) -> Node "PatBind"
  (VarBind  _a _b _c       )   -> Node "VarBind"
  -- (AbsBinds tvs evvars exports _evbinds binds) -> Node "AbsBinds" ++ (sie tvs) ++ (sie evvars) ++ (concatMap showExport exports) {- ++ (sie evbinds) -}  ++ (concatMap (\b -> myShow $ unLoc b) $ bagToList binds)

-- getMatchGroup :: Show t => MatchGroup t -> String
getMatchGroup (MatchGroup ms _ptc) = map (\m -> getMatch $ unLoc m) ms

-- getMatch (Match [LPat id] (Maybe (LHsType id)) (GRHSs id))
getMatch (Match lpats mt rhs) = NodeMatch (map getLPat lpats) Nothing (getGRHSs rhs)

getLPat _ = Node "(LPat)"

getGRHSs (GRHSs gs lb) = NodeGRHSs (map getLGRHS gs) (getHsLocalBinds lb)

getLGRHS lg = getGRHS $ unLoc lg

getGRHS (GRHS ls e) = NodeGRHS (map getLStmt ls) (getLHSExpr e)

getLStmt ls = getStmtLR $ unLoc ls

getStmtLR s = case s of
  LastStmt l s {- (LHsExpr idR) (SyntaxExpr idR) -} -> Node "(LastStmt)"
  BindStmt lp l s1 s2 {- (LPat idL) (LHsExpr idR) (SyntaxExpr idR) (SyntaxExpr idR) -} -> Node "(BindStmt)"
  ExprStmt e s1 s2 pt {- (LHsExpr idR) (SyntaxExpr idR) (SyntaxExpr idR) PostTcType -} -> Node "(ExprStmt)"
  LetStmt lb  {- (HsLocalBindsLR idL idR) -} -> Node "(LetStmt)"
  ParStmt a b c d {- [([LStmt idL], [idR])] (SyntaxExpr idR) (SyntaxExpr idR) (SyntaxExpr idR) -} -> Node "(ParStmt)"
  TransStmt a b c d e f g h -> Node "(TransStmt)"
  RecStmt a b c d e f g h i -> Node "(RecStmt)"

getLHSExpr le = getHSExpr $ unLoc le

getHSExpr e = case e of
  HsVar vid -> Node $ "(HsVar " ++ "(show vid)" ++ ")"
  HsIPVar _id -> Node "(HsIPVar)"
  HsOverLit (OverLit v _rb _w _t) -> Node $ "(HsOverLit " ++ "(showOverLit v)" ++ ")"
  HsLit _lit -> Node "(HsLit)"
  HsLam _mg -> Node "(HsLam)"
  HsApp _e1 _e2 -> Node "(HsApp)"
  OpApp e1 e2 _fixity e3 -> Node $ "(OpApp:" ++ "(showHsExpr $ unLoc e1)" ++ "," ++ "(showHsExpr $ unLoc e2)" ++ "," ++ "(showHsExpr $ unLoc e3)" ++ ")"
  NegApp _e1 _se1 -> Node "(NegApp)"
  HsPar _e1 -> Node "(HsPar)"
  SectionL _e1 _e2 -> Node "(SectionL)"
  SectionR _e1 _e2 -> Node "(SectionR)"
  ExplicitTuple _tuparg _boxity -> Node "(ExplicitTuple)"
  HsCase _e1 _mg -> Node "(HsCase)"
  HsIf _mse _e1 _e2 _e3 -> Node "(HsIf)"
  HsLet _lb _e1 -> Node "(HsLet)"
  HsDo n ls t -> Node $ "(HsDo"++"(showHsStmtContext n)" ++ "," ++"(concatMap showLStmt ls)"++","++"(showPostTcType t)"++")"
  ExplicitList _t _e1 -> Node "(ExplicitList)"
  ExplicitPArr _t _e1 -> Node "(ExplicitPArr)"
  RecordCon _li _te _rb -> Node "(RecordCon)"
  RecordUpd _e1 _rb _dcs _ts1 _ts2 -> Node "(RecordUpd)"
  ExprWithTySig _e1 _t -> Node "(ExprWithTySig)"
  ExprWithTySigOut _e1 _t -> Node "(ExprWithTySigOut)"
  ArithSeq _te _si -> Node "(ArithSeqInfo)"
  PArrSeq _te _si -> Node "(PArrSeq)"
  HsSCC _fs _e1 -> Node "(HsSCC)"
  HsCoreAnn _fs _e1 -> Node "(HsCoreAnn)"
  HsBracket _b -> Node "(HsBracket)"
  HsBracketOut _b _ps -> Node "(HsBracketOut)"
  HsSpliceE _s -> Node "(HsSplice)"
  HsQuasiQuoteE _qq -> Node "(HsQuasiQuote)"
  HsProc _lp _lc -> Node "(HsProc)"
  HsArrApp _e1 _e2 _t _at _b -> Node "(HsArrAppType)"
  HsArrForm _e1 _mf _lcs -> Node "(HsArrForm)"
  HsTick _ti _e1 -> Node "(HsTick)"
  HsBinTick _i1 _i2 _e1 -> Node "(HsBinTick)"
  HsTickPragma _a _b -> Node "(HsTickPragma)"
  EWildPat -> Node "(EWildPat)"
  EAsPat _lid _e1 -> Node "(EAsPat)"
  EViewPat _e1 _e2 -> Node "(EViewPat)"
  ELazyPat _e1 -> Node "(ELazyPat)"
  HsType _lt -> Node "(HsType)"
  HsWrap _w e1 -> Node $ "(HsWrap:(HsWrapper)," ++ "(showHsExpr e1)" ++ ")"
  -- _         -> Node "(unk HsExpr)"




getHsLocalBinds _ = Node "HsLocalBinds"
