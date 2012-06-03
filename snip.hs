{-

Copyright (C) 2012 Tim Cuthbertson

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.


Original directory-scanning code from:
http://blog.moertel.com/articles/tag/directory_tree_series
(C) Tom Moertel <tom@moertel.com> 2007
(Licenced under the GPL)

-}

module Main (main) where

import Prelude hiding (catch)
import qualified Prelude
import Control.Monad
import Control.Applicative ((<|>))
import Control.Exception (catch, SomeException)
import System.Exit (exitFailure)
import Data.List
import Data.Tree
import Data.Maybe (isJust)
import System.FilePath
import System.Directory
import System.Environment
import System.IO
import System.Process (readProcess)

#ifndef MINIMAL
import qualified System.Console.ANSI as Term
#endif

import System.IO.Unsafe


-- Some convenient type synonyms

type QueryPath  = [String]         -- arguments given by user, which may not correspond directly to a path
type TreePath   = [DirName]        -- path components from the root of the tree (i.e folder names)
type DirName    = String           -- directory-entry name
type DirTree    = Tree Item        -- file-system tree
data Item = Item                   -- a snippet item (node in the tree)
	{ filepath :: FilePath
	, name     :: String
	, contents :: ItemContents
	} deriving (Show)

data ItemContents =                -- an item's contents, which will either be sub-items or an actual snippet
	Folder [DirName] |
	File String deriving Show


-- High-level program logic: process args and perform the appropriate action

main :: IO ()
main = do
	args <- getArgs
	run args

run :: [String] -> IO ()
run args = do
	(run' args) `catch` errorAction
	where
		errorAction err = showError (show (err :: SomeException))
		showError :: String -> IO ()
		showError err = do
			outLn [red err]
			exitFailure

run' :: [String] -> IO ()
run' args = do
	let (action, query) = parseArgs args
	homedir <- getHomeDirectory
	let base = joinOne homedir ".snip"
	createDirectoryIfMissing True base
	tree <- fsTraverse base
	action query tree

type Action = QueryPath -> DirTree -> IO ()
type LookupAction = (TreePath, DirTree) -> IO ()

actions = [
		("get",     actionGet,     Just "print the contents of a snippet or list"),
		("set",     actionSet,     Just "set the contents of a snippet (create or update)"),
		("copy",    actionCopy,    Just "copy the contents of a snippet to the clipboard (requires `pyperclip`)"),
		("all",     actionAll,     Just "list all snippets"),
		("rm",      actionRemove,  Just "remove a snippet or list"),
		("which",   actionWhich,   Just "show the path to a snippet"),
		("--help",  actionHelp,    Nothing)
	]

parseArgs :: [String] -> (Action, QueryPath)
parseArgs args = explitAction `getOrElse` defaultAction
	where
		explitAction = fmap useRemainingArgs $ parseAction (maybeHead args)
		useRemainingArgs action = (action, tail args)
		defaultAction = (actionGet, args)

parseAction :: Maybe String -> Maybe Action
parseAction Nothing = Just actionHelp
parseAction (Just x) = fmap getAction $ find matchingAction actions
	where
		getAction (_, action, _) = action
		getActionName (name, _, _) = name
		matchingAction = (== x) . getActionName

actionGet :: Action
actionGet = lookupAction $ render . pp . getSnippetContents

actionCopy :: Action
actionCopy search = lookupAction (\node -> copyToClipboard search $ getSnippetContents node) search

actionSet :: Action
actionSet args tree = do
	let relpath = init args
	let relbase = init relpath
	let path = addToRoot tree relpath
	let base = addToRoot tree relbase
	baseExists <- doesDirectoryExist base
	unless baseExists (promptAndCreate relbase tree)
	val <- getInput $ last args
	writeFile path val
	outLn [plain "Got it. ", yellow (joinPath relpath),  plain " is now ", green val]

promptAndCreate args tree = do
	continue <- (confirm $ "Folder " ++ (joinPath args) ++ " does not exist. create it?")
	unless continue $ fail "Cancelled."
	actionNew args tree
	return ()

actionNew :: Action
actionNew args tree = do
	let path = addToRoot tree args
	createDirectoryIfMissing True path
	outLn [plain "Created: ", green (joinPath args)]

actionWhich :: Action
actionWhich = lookupAction (putStrLn . joinPath . fst)

actionRemove :: Action
actionRemove = lookupAction doRemove where
	doRemove (path, node) = do
		let path' = joinPath path
		continue <- confirm $ "Really delete " ++ path' ++ "?"
		when continue (rm path' (contents $ rootLabel node) >> outLn [plain "Deleted ", red path'])
	rm path (Folder _) = removeDirectoryRecursive path
	rm path (File _  ) = removeFile path

lookupAction :: LookupAction -> Action
lookupAction action search tree = success `getOrElse` (fail $ "Could not find " ++ (describeSearch search))
	where
		success = fmap action (resolvePath search tree)

actionAll :: Action
actionAll [] tree = render $ showTree tree
actionAll args _ = fail "too many arguments for `all`"

actionHelp :: Action
actionHelp args tree = render generalHelp
	where
		generalHelp = describeActions ++ [newLn] ++ describeLists
		helpOn (name, _, (Just desc)) = appendLn $ pad name ++ [plain desc]
		helpOn (name, _, Nothing) = []
		pad name = [helpActionColor ("  " ++ justifyLeft 5 name), plain " : "]
		describeActions = [plain "Actions:", newLn] ++ (concat $ map helpOn actions)
		describeLists = (plain "Your lists:") : newLn : pp (contents $ rootLabel tree)

-- action helpers / workers

addToRoot :: DirTree -> QueryPath -> FilePath
addToRoot tree path = joinPath ([name $ rootLabel tree] ++ path)

copyToClipboard search item = do
	output <- readProcess "pyperclip" ["--copy"] (up item)
	putStr output -- should be empty, but just in case?
	outLn [plain "Copied ", yellow (describeSearch search), plain " to your clipboard"]

describeSearch :: QueryPath -> String
describeSearch = joinPath

getSnippetContents :: (TreePath, DirTree) -> ItemContents
getSnippetContents = contents . rootLabel . snd

-- tree traversal / searching

firstMatch :: [Maybe a] -> Maybe a
firstMatch items = join $ find isJust items

resolvePath :: TreePath -> DirTree -> Maybe (TreePath, DirTree)
resolvePath path tree = foldl resolveOne (Just ([name $ rootLabel tree], tree)) path

resolveOne :: Maybe (TreePath, DirTree) -> DirName -> Maybe (TreePath, DirTree)
resolveOne start needle = join $ fmap applyFind start
	where
		applyFind :: (TreePath, DirTree) -> Maybe (TreePath, DirTree)
		applyFind = uncurry (findBF' needle)


-- find node by name in a breadth-first manner.
-- returns the path to the node, and the subtree at that point
findBF :: DirName -> DirTree -> Maybe (TreePath, DirTree)
findBF needle tree = findBF' needle [] tree
findBF' :: DirName -> TreePath -> DirTree -> Maybe (TreePath, DirTree)
findBF' needle prefix tree = topLevelMatch <|> findNextLevel
	where
		topLevelMatch = findInLevel needle prefix (subForest tree)
		findNextLevel = firstMatch $ map subFindBF (subForest tree)
		subFindBF :: DirTree -> Maybe (TreePath, DirTree)
		subFindBF node = findBF' needle (prefix `ncons` node) node
		findInLevel :: DirName -> TreePath -> Forest Item -> Maybe (TreePath, DirTree)
		findInLevel needle prefix nodes = fmap (returnTree prefix) found
			where
				found :: Maybe DirTree
				found = find (((==) needle) . name . rootLabel) nodes

returnTree :: TreePath -> DirTree -> (TreePath, DirTree)
returnTree prefix node = (ncons prefix node, node)

-- path manipulation

ncons :: TreePath -> DirTree -> TreePath
ncons xs x = xs `pcons` (name $ rootLabel x)

pcons :: TreePath -> String -> TreePath
pcons xs x = xs ++ [x]

-- Effectful tree-builder for file-system hierarchies

joinOne a b = joinPath [a,b]

fsTraverse :: FilePath -> IO DirTree
fsTraverse path = (lazyUnfoldTreeM fsTraverseStep) (path, path)

fsTraverseStep :: (FilePath,String) -> IO (Item, [(FilePath,String)])
fsTraverseStep node@(path, name) = do
	item <- mkItem path name
	let children = getChildren $ contents item
	return (item, children)
	where
		getChildren (File _) = []
		getChildren (Folder children) = map (\name -> (joinOne path name, name)) children

mkItem :: FilePath -> String -> IO Item
mkItem path name = do
	contents <- getContents path
	return $ Item { filepath=path, name=name, contents=contents}
	where
		getContents :: FilePath -> IO ItemContents
		getContents path = do
			isdir <- doesDirectoryExist path
			(if isdir then getFolder else getFile) path
		getFile :: FilePath -> IO ItemContents
		getFile path = readFile path >>= return . File
		getFolder :: FilePath -> IO ItemContents
		getFolder path = getVisibleDirectoryContents path >>= return . Folder

getVisibleDirectoryContents :: FilePath -> IO [FilePath]
getVisibleDirectoryContents path = getDirectoryContents path >>=
	return . sort . filter (`notElem` [".", ".."])

lazyUnfoldTreeM :: (b -> IO (a, [b])) -> b -> IO (Tree a)
lazyUnfoldTreeM step seed = do
	(root, seeds) <- step seed
	children <-
		unsafeInterleaveIO $
		mapM (lazyUnfoldTreeM step) seeds
	return (Node root children)

-- Purely functional tree-to-string formatting

showTree :: DirTree -> Output
showTree t = concat $ map (showNode []) (subForest t)

showNode :: TreePath -> DirTree -> Output
showNode path tree =
	nodeRep ++ childRep
	where
		node = rootLabel tree
		nodeName = name $ node
		nodeRep  = appendLn $ [plain prefix] ++ summarizeNode node
		prefix = (replicate (2 * length path) ' ') ++ " - "
		childRep = concat $ map (showNode (path ++ [nodeName])) (subForest tree)

summarizeNode Item { name=name, contents = (Folder _) } = [snippetFolderColor name]
summarizeNode Item { name=name, contents = (File str) } = [snippetNameColor name, plain ": "] ++ ellipsize summary
	where
		max = 50
		summary = take max (takeWhile (/= '\n') str)
		ellipsize summary = if summary == str then [plain summary] else [plain $ (take (max-3) summary) ++ "..."]

-- pretty print
pp :: ItemContents -> Output
pp (Folder []) = [yellow "(empty list)"]
pp (Folder contents) = concat $ map (\name -> [plain " - ", yellow name, newLn]) contents
pp (File contents) = appendLn [plain contents]

-- ugly print
up :: ItemContents -> String
up (Folder contents) = unlines contents
up (File contents) = contents

getOrElse Nothing b = b
getOrElse (Just a) _ = a

maybeHead :: [a] -> Maybe a
maybeHead [] = Nothing
maybeHead (x:xs) = Just x

justifyLeft :: Int -> String -> String
justifyLeft n str = str ++ replicate (max 0 (n - length str)) ' '

confirm :: String -> IO Bool
confirm question = do
	putStr $ question ++ " [Y/n] "
	hFlush stdout
	answer <- getLine
	return (answer `elem` ["","y","Y"])

getInput "-" = do
	putStrLn "(reading from stdin, ctrl+d to finish)"
	getContents
getInput val = return val


#ifndef MINIMAL
-- Colored formatting depends on System.Console.ANSI, which
-- is NOT guaranteed to be available when running interactively (via runghc)
type TextAtom = (Maybe Term.Color, String)
color :: Term.Color -> a -> (Maybe Term.Color, a)
color = (,) . Just
plain = (,) Nothing
red = color Term.Red
blue = color Term.Blue
cyan = color Term.Cyan
green = color Term.Green
yellow = color Term.Yellow

renderTextAtom :: TextAtom -> IO ()
renderTextAtom = (uncurry renderTextAtom') . convertTextAtom where
	renderTextAtom' :: [Term.SGR] -> String -> IO ()
	renderTextAtom' _ "\n" = putStrLn ""
	renderTextAtom' modes str = do
		isTTY <- hIsTerminalDevice stdout
		when isTTY $ Term.setSGR modes
		putStr str
		when isTTY $ Term.setSGR []

convertTextAtom :: TextAtom -> ([Term.SGR], String)
convertTextAtom (Nothing, str) = ([], str)
convertTextAtom (Just col, str) = ([Term.SetColor Term.Foreground Term.Dull col], str)
#else
type TextAtom = String
plain = id
red = id
blue = id
cyan = id
green = id
yellow = id

renderTextAtom :: TextAtom -> IO ()
renderTextAtom "\n" = putStrLn ""
renderTextAtom s = putStr s
#endif

-- meaningful colour aliases
snippetNameColor = green
snippetFolderColor = yellow
helpActionColor = cyan

type Output = [TextAtom]

render :: Output -> IO ()
render outputs = mapM_ renderTextAtom outputs

appendLn t = t ++ [newLn]
newLn = plain "\n"
outLn = render . appendLn
