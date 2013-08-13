import System.Environment
import System.IO
import System.Directory
import Data.List
import Control.Exception
import Control.Applicative

main = do
  (command:args) <- getArgs
  dispatch command args

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

showUsage :: IO ()
showUsage = putStr $ concat $ (++ "\n") <$> [
    "Usage:",
    "  add [task content]",
    "  list",
    "  remove [index of task]"
  ]
