import System.IO
import System.Directory(getTemporaryDirectory, removeFile)
import System.IO.Error(catch)
import Control.Exception(finally)

main:: IO ()
main = withTempFile "mytemp.txt" myAction

myAction :: FilePath -> Handle -> IO ()
myAction tempname temph =
    do
        putStrLn "welcome to tempfile.hs"
        putStrLn "I have a temporary file @" ++ tempname

        pos <- hTell temph
        putStrLn $ "my init position is" ++ show pos

        let tempdata = show [1..10]
        putStrLn $ "writing one line containing " ++
            show (length tempdata) ++ " bytes:" ++ 
            tempdata
        hPutStrLn temph tempdata

        pos <- hTell temph
        putStrLn $ "After writing, my new position is " ++ show pos

        putStrLn $ "the file content is: "
        hSeek temph AbsoluteSeek 0

        c <- hGetContents temph
        putStrLn c
        putStrLn $ "which could be expressed as haskell literal:"
        print c

withTempFile :: String -> (FilePath -> Handle -> IO a) -> IO a
withTempFile pattern func =
    do
        tempdir <- catch (getTemporaryDirectory) (\_ -> return ".")
        (tempfile, temph) <- openTempFile tempfile pattern
        finally (func tempfile temph)
            (do hClose temph
                removeFile tempfile)