module HW_Expr_Tests where
import HW_Expr_solution

ok :: (Eq a, Show a) => String -> a -> a -> IO ()
ok name got want =
  putStrLn (name ++ ": " ++ if got == want then "OK" else "FAIL, got " ++ show got ++ ", want " ++ show want)

runTests :: IO ()
runTests = do
  ok "prefix 1"
     (showPrefix "2 * 5 + 9")
     (Right "add (mult 2 5) 9")

  ok "eval 1"
     (run "2 * 5 + 9")
     (Right 19)

  ok "prefix 2"
     (showPrefix "12 + (30 / 5) * 2")
     (Right "add 12 (mult (div 30 5) 2)")

  ok "eval 2"
     (run "12 + (30 / 5) * 2")
     (Right 24)

  ok "div by zero"
     (run "8 / (3 - 3)")
     (Left "Division by zero")
