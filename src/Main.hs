import Control.Applicative
import Control.Exception
import Control.Monad
import Data.List
import System.Directory
import System.Environment
import System.FilePath
import System.IO

main = do
  (command:args) <- getArgs
  home           <- getHomeDirectory
  let path        = home </> "data/todo.hs.txt"

  ensureDataFile path

  dispatch command (path:args)

dispatch :: String -> [String] -> IO ()
dispatch "add"    = addTask
dispatch "list"   = listTasks
dispatch "remove" = removeTask
dispatch _        = \_ -> showUsage

addTask :: [String] -> IO ()
addTask [path, task] = appendFile path (task ++ "\n")

listTasks :: [String] -> IO ()
listTasks [path] = do
  contents <- readFile path

  let tasks          = lines contents
      tasksWithIndex = zipWith (\i line -> show i ++ ":\t" ++ line) [0..] tasks

  putStr $ unlines tasksWithIndex

removeTask :: [String] -> IO ()
removeTask [path, indexStr] = do
  contents <- readFile path

  let tasks       = lines contents
      index       = read indexStr
      newContents = unlines $ delete (tasks !! index) tasks

  bracketOnError (openTempFile "." "temp")
    (\(tempPath, tempHandle) -> do
      hClose tempHandle
      removeFile tempPath)
    (\(tempPath, tempHandle) -> do
      hPutStr tempHandle newContents
      hClose tempHandle
      removeFile path
      renameFile tempPath path)

ensureDataFile path = doesFileExist path >>= \exists ->
  unless exists $ do
    writeFile path ""

showUsage :: IO ()
showUsage = putStr $ concat $ (++ "\n") <$> [
    "Usage:",
    "  add [task content]",
    "  list",
    "  remove [index of task]"
  ]
