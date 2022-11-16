#!/usr/bin/env runghc
import System.Environment (getArgs)
import qualified Data.List as L

-- Generate Futhark code.

perms :: Int -> Int -> [([Int], [Int])]
perms n k = do
  let is = [0..n-1]
  p0 <- is
  prest <- f (k - 1) p0
  let ps = p0 : prest
      ps_inv = filter (`notElem` ps) is
  return (ps, ps_inv)

  where f 0 _ = return []
        f i prev = do
          cur <- [prev+1..n-1]
          next <- f (i - 1) cur
          return $ cur : next

gen :: Int -> Int -> String
gen n k =
  let body =
        L.intercalate "\n  + "
        $ map (\(ps, ps_inv) ->
                 L.intercalate " * "
                (map p ps ++ map ((\s -> "(1 - " ++ s ++ ")") . p) ps_inv))
        $ perms n k
  in "-- gen " ++ show n ++ " " ++ show k ++ "\ndef calc " ++ L.intercalate " " (map ((\s -> "(" ++ s ++ ": f32)") . p) [0..n-1]) ++ ": f32 =\n  " ++ body
  where p = ("p" ++) . show

main :: IO ()
main = do
  args <- getArgs
  case args of
    [n, k] -> putStrLn $ gen (read n) (read k)
    _ -> fail "invalid input"
