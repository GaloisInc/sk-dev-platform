import System.Posix.Interaction(Interaction(..), OStream(..), testProgram)

-- throw-away example demonstrating Interactions

main :: IO ()
main = testProgram "/usr/bin/head" ["-1"] Nothing True (Interactions [Feed "apa\n", Expect (Just 2) StdOut "apa\n"])
