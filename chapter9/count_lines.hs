import System.Environment
import System.IO
import System.IO.Error
import Control.Exception

main = toTry `catch` handler

toTry :: IO ()
toTry = do  (fileName:_) <- getArgs
            contents <- readFile fileName
            putStrLn $ "The file has " ++ show (length (lines contents)) ++ " lines!"

handler :: IOError -> IO ()
handler e
    | isDoesNotExistError e = putStrLn $ "Whoops! File doesn't exist at" ++ showPath
    | otherwise = ioError e
        where showPath = case ioeGetFileName e of Just path -> ": " ++ path
                                                  Nothing -> " unknown location!"
