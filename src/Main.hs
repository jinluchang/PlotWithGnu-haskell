module Main where

import Control.Applicative
import System.Environment

import Graphics.PlotWithGnu

parseData :: String -> [[Double]]
parseData = (map . map) read' . map words . map go . lines where
    go = takeWhile (/= '#')
    read' str = case reads str of
        [] -> 0
        (val,_):_ -> val

main :: IO ()
main = do
    args <- getArgs
    switchAction args

switchAction :: [String] -> IO ()
switchAction [lattr] = do
    dataTable <- parseData <$> getContents
    plotview [] "plot [] []" [(dataTable, lattr)]

switchAction _ = switchAction [""]
