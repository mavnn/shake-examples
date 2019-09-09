import Generated.Fighter
import Names

main :: IO ()
main = do
  putStrLn $ "Hello, " ++ world ++ "!"
  print
    ( Fighter
      { insaneToughness = 5
      , ridiculousStrength = 10
      }
    )
