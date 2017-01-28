module Main
where
import System
import IO
import CPlusPlusLexer
import CPlusPlusParser



main = do putStrLn "test-c++-parser"
	  args <- getArgs
	  case args of
		    [fname1] -> do putStrLn ("parsing " ++ fname1)
				   str <- readFile fname1
				   runLex declaration str
				   return ()
		    _ -> putStrLn "usage: test-c++-parser <fname>"
