import Data.List
import Data.List.Split

import System.Environment
import System.IO
import System.Random


mapLines :: (String -> String) -> String -> String
mapLines someFn contents =
  unlines . map someFn . lines $ contents

dispatch :: String -> String -> String
dispatch "reverse" contents =
  mapLines reverse contents
dispatch "space" contents =
  mapLines (intercalate " " . chunksOf 1) contents
dispatch "reverse-lines" contents =
  (unlines . reverse . lines) contents
dispatch cmd _ =
  error "No such command " ++ cmd 

-- This is where the show begins!
main = do
  args <- getArgs
  let argsReversed = reverse args
      fileIn = argsReversed !! 1
      fileOut = argsReversed !! 0
      commands = (reverse . drop 2) argsReversed
  contents <- readFile fileIn
  writeFile fileOut $ foldl (\contents' cmd -> dispatch cmd contents') contents commands
                                 
