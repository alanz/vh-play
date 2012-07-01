
-- Prelude Control.Monad.Reader Language.Haskell.Exts> (


--import GHC
--import GHC.Paths ( libdir )
--import DynFlags ( defaultDynFlags )


import Control.Monad.Reader
import Language.Haskell.Exts



{-
main =
    defaultErrorHandler defaultDynFlags $ do
      runGhc (Just libdir) $ do
        dflags <- getSessionDynFlags
        setSessionDynFlags dflags
        target <- guessTarget "test_main.hs" Nothing
        setTargets [target]
        load LoadAllTargets
-}


foo = (liftM prettyPrint) $
      (liftM fst) $ (liftM fromParseResult) $
      parseFileWithComments defaultParseMode "./src/A.hs"
