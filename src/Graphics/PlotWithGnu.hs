{-# LANGUAGE QuasiQuotes, ForeignFunctionInterface #-}
module Graphics.PlotWithGnu where

import Data.List
import Control.Monad
import System.IO
import System.Process
import System.Directory
import System.FilePath
import System.Directory
import System.Posix.Temp
import Data.String.QQ

import qualified Data.ByteString.Char8 as B
import Foreign.C.Types
import Foreign.C.String
import System.IO.Unsafe

type DataTable = [[Double]]

select :: [Int] -> [a] -> [a]
select [] _ = []
select (n:ns) xs = xs !! n : select ns xs

transpose'               :: [[a]] -> [[a]]
transpose' []             = []
transpose' ([]     : _)   = [] -- try to make transpose memory efficient
transpose' ((x:xs) : xss) = (x : [h | (h:_) <- xss]) : transpose' (xs : [ t | (_:t) <- xss])

saveDataTable :: FilePath -> DataTable -> IO ()
saveDataTable filename table = writeFile filename $ showDataTable table

loadDataTable :: FilePath -> IO DataTable
loadDataTable filename = readFile filename >>= return . readDataTable

loadDataTable' :: FilePath -> IO DataTable
loadDataTable' filename = B.readFile filename >>=
    return . (map . map) unsafeReadDouble . map B.words .
    filter (not . isComment) . B.lines
  where
    isComment l = B.null l || B.head l == '#'

unsafeReadDouble :: B.ByteString -> Double
unsafeReadDouble str = unsafePerformIO $ B.useAsCString str c_atof

foreign import ccall unsafe "stdlib.h atof" c_atof :: CString -> IO Double

showTable :: [[String]] -> String
showTable = unlines . map unwords . transpose . go . transpose where
    go = map padStrs
    padStrs strs = map (padStr w) strs where
        w = maximum $ map length strs
    padStr w str = replicate padLen ' ' ++ str where
        padLen = w - length str

showTable' :: [[String]] -> String
showTable' = unlines . map unwords

readTable :: String -> [[String]]
readTable = map words . filter (not . isComment) . lines where
    isComment l = head l == '#'

showDataTable :: DataTable -> String
showDataTable = showTable . (map . map) show

readDataTable :: String -> DataTable
readDataTable = (map . map) read . readTable

createDataFile :: Int -> DataTable -> IO FilePath
createDataFile _ [] = return ""
createDataFile id table = do
    saveDataTable dataFile table
    return dataFile
  where
    dataFile = "data-file-" ++ show id

type PlotFile = String

mkPlotFile :: FilePath -> [String] -> String -> [(String, String)] -> PlotFile
mkPlotFile fn settings plot plotlines = unlines $
    [ mpTermHeader
    , mpSetOutput fn ]
    ++ settings ++
    [ plot ++ " \\"
    , intercalate ", \\\n" $ map go plotlines ]
  where
    go ("", attr) = "  " ++ attr
    go (dataFile, attr) = "  " ++ show dataFile ++ " " ++ attr

mpTermHeader :: PlotFile
mpTermHeader = "set term mp color latex prologues 3 amstex"

mpSetOutput :: FilePath -> PlotFile
mpSetOutput fn = "set output '" ++ fn ++ ".mp'"

runGnuplotMp :: PlotFile -> IO ()
runGnuplotMp input = do
    writeFile "plotfile" input
    hlogGnuplot <- openFile "log" AppendMode
    void $ runProcess "gnuplot" ["plotfile"] Nothing Nothing Nothing (Just hlogGnuplot) Nothing
        >>= waitForProcess
    hClose hlogGnuplot
    writeFile "convert.sh" commandMpToEps
    hlogConvert <- openFile "log" AppendMode
    void $ runProcess "bash" ["convert.sh"] Nothing Nothing Nothing (Just hlogConvert) Nothing
        >>= waitForProcess
    hClose hlogConvert

commandMpToEps :: String
commandMpToEps = [s|
for i in *.mp ; do
	fn=${i%.mp}
	echo $fn
	TEX=latex mpost -jobname mpost-job $fn.mp
	mv mpost-job.0 $fn.eps
done
|]

gnuplot :: FilePath -> [String] -> String -> [(DataTable, String)] -> IO FilePath
gnuplot fn settings plot datalines = do
    tempdirP <- getTemporaryDirectory
    tempdir <- mkdtemp $ tempdirP </> "plot-with-gnu-"
    setCurrentDirectory tempdir
    dataFiles <- zipWithM createDataFile [1..] dataTables
    let plotfile = mkPlotFile fn settings plot $ zip dataFiles $ map snd datalines
    runGnuplotMp plotfile
    -- mapM_ removeFile dataFiles
    return $ tempdir </> replaceExtension fn ".eps"
  where
    dataTables = map fst datalines

viewEps :: FilePath -> IO ()
viewEps path = void . system $ "evince " ++ path ++ " &"

saveFile :: FilePath -> FilePath -> IO ()
saveFile old new = void . system $ "mv '" ++ old ++ "' '" ++ new ++ "'"

plotview :: [String] -> String -> [(DataTable, String)] -> IO ()
plotview settings plot datalines = do
    pwd <- getCurrentDirectory
    fn <- gnuplot "PlotWithGnu" settings plot datalines
    setCurrentDirectory pwd
    viewEps fn

plotsave :: FilePath -> [String] -> String -> [(DataTable, String)] -> IO ()
plotsave fn settings plot datalines = do
    pwd <- getCurrentDirectory
    fn' <- gnuplot "PlotWithGnu" settings plot datalines
    setCurrentDirectory pwd
    saveFile fn' fn
