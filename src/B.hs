--B.hs
module B where

main = print "Hello, World!"

foo x = bar * x

bar = 4 + z
  where
    z = 2

x = 1
