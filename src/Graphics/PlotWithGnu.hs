{-# LANGUAGE QuasiQuotes #-}
module Graphics.PlotWithGnu where

import Data.List
import System.IO
import System.Process
import System.Directory
import Control.Monad
import System.FilePath
import System.Posix.Temp
import Data.String.QQ

type PlotFile = String

type DataTable = [[Double]]

mkPlotFile :: FilePath -> [String] -> String -> [(String, String)] -> PlotFile
mkPlotFile fn settings plot plotlines = unlines $
    [ mpTermHeader
    , mpSetOutput fn ]
    ++ settings ++ 
    [ plot ++ " \\"
    , intercalate ", \\\n" $ map go plotlines ]
  where
    go (dataFile, attr) = "  " ++ show dataFile ++ " " ++ attr

mpTermHeader :: PlotFile
mpTermHeader = "set term mp color latex prologues 3 amstex"

mpSetOutput :: FilePath -> PlotFile
mpSetOutput fn = "set output '" ++ fn ++ ".mp'"

createDataFile :: FilePath -> DataTable -> IO FilePath
createDataFile tempdir table = do
    (dataFile, hDataFile) <- openTempFile tempdir "data-file-"
    hPutStr hDataFile contents
    hClose hDataFile
    return dataFile
  where
    contents = showTable . (map . map) show $ table

showTable :: [[String]] -> String
showTable = unlines . map unwords . transpose . go . transpose where
    go = map padStrs
    padStrs strs = map (padStr w) strs where
        w = maximum $ map length strs
    padStr w str = replicate padLen ' ' ++ str where
        padLen = w - length str

showTable' :: [[String]] -> String
showTable' = unlines . map unwords

runGnuplotMp :: FilePath -> PlotFile -> IO ()
runGnuplotMp tempdir input = do
    writeFile (tempdir </> "plotfile") input
    hlogGnuplot <- openFile (tempdir </> "log") AppendMode
    void $ runProcess "gnuplot" ["plotfile"] (Just tempdir) Nothing Nothing (Just hlogGnuplot) Nothing
        >>= waitForProcess
    hClose hlogGnuplot
    writeFile (tempdir </> "convert.sh") commandMpToEps
    hlogConvert <- openFile (tempdir </> "log") AppendMode
    void $ runProcess "bash" ["convert.sh"] (Just tempdir) Nothing Nothing (Just hlogConvert) Nothing
        >>= waitForProcess
    hClose hlogConvert

commandMpToEps :: String
commandMpToEps = [s|
for i in *.mp ; do
	fn=${i%.mp}
	echo $fn
	mpost -jobname mpost-job $fn.mp
	mv mpost-job.0 $fn.eps
done
|]

gnuplot :: FilePath -> [String] -> String -> [(DataTable, String)] -> IO FilePath
gnuplot fn settings plot datalines = do
    tempdirP <- getTemporaryDirectory
    tempdir <- mkdtemp $ tempdirP </> "plot-with-gnu-"
    dataFiles <- mapM (createDataFile tempdir) dataTables
    let plotfile = mkPlotFile fn settings plot $ zip dataFiles $ map snd datalines
    runGnuplotMp tempdir plotfile
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
    fn <- gnuplot "PlotWithGnu" settings plot datalines
    viewEps fn

plotsave :: FilePath -> [String] -> String -> [(DataTable, String)] -> IO ()
plotsave fn settings plot datalines = do
    fn' <- gnuplot "PlotWithGnu" settings plot datalines
    saveFile fn' fn
