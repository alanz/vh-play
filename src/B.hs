--B.hs
module B where

-- main = print "Hello, World!"

-- foo :: Integer -> Integer
-- foo x = bar * x

a = 3

bar = 4 + z
  where
    z :: Int
    z = 2

x = 1 + 2

f = do
  putStr "What is your first name? "
  first <- getLine
  return first

{-
,AbsBinds[][](ABE B.f,f)(FunBind:f[((Match [] (GRHSs [(GRHS [] (HsDo))] EmptyLocalBinds )))])
,AbsBinds[][](ABE B.x,x)(FunBind:x[((Match [] (GRHSs [(GRHS [] (OpApp:(HsOverLit 1),(HsWrap:(HsWrapper),(HsVar GHC.Num.+)),(HsOverLit 2)))] EmptyLocalBinds )))]),AbsBinds[][](ABE B.bar,bar)(FunBind:bar[((Match [] (GRHSs [(GRHS [] (OpApp:(HsOverLit 4),(HsWrap:(HsWrapper),(HsVar GHC.Num.+)),(HsVar z)))] (HsValBinds:(ValBindsOut:(recFlag,,AbsBinds[][](ABE z,z)(FunBind:z[((Match [] (GRHSs [(GRHS [] (HsOverLit 2))] EmptyLocalBinds )))])),(TypeSig [z],(HsTyVar Int)))) )))])

-}


{-
,AbsBinds[][]
  (ABE B.x,x)
  (FunBind:x[
    ((Match []
      (GRHSs [(GRHS []
               (OpApp:(HsOverLit 1),
                      (HsWrap:(HsWrapper),(HsVar GHC.Num.+)),
                      (HsOverLit 2)))]
             EmptyLocalBinds )))])

,AbsBinds[][]
  (ABE B.bar,bar)
   (FunBind:bar[
     ((Match []
       (GRHSs [(GRHS []
         (OpApp:(HsOverLit 4),
                (HsWrap:(HsWrapper),(HsVar GHC.Num.+)),
                (HsVar z)))]
         (HsValBinds:
            (ValBindsOut:
              (recFlag,
              ,AbsBinds[][](ABE z,z)
                (FunBind:z[
                  ((Match []
                    (GRHSs [
                      (GRHS [] (HsOverLit 2))]
                     EmptyLocalBinds )))])),
                (TypeSig [z],(HsTyVar Int)))) )))])

-}