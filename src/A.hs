--A.hs
--invoke: ghci -package ghc A.hs

import GHC
import Outputable
import Bag
import GHC.Paths ( libdir )
--GHC.Paths is available via cabal install ghc-paths

import DynFlags

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
  let lhsbinds = bagToList res
  -- putStrLn $ showSDoc ( ppr res )
  putStrLn $ showSDoc ( ppr lhsbinds )





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