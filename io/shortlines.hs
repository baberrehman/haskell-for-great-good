
main = do
  contents <- getContents
  putStr $ shortLinesOnly contents


shortLinesOnly :: String -> String
shortLinesOnly input = unlines $ shortLines
                         where allLines = lines input
                               shortLines = filter (\line -> length line < 10) allLines