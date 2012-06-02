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

type QueryPath  = [String]
type TreePath   = [DirName]
type DirName    = String           -- directory-entry name
type DirTree    = Tree DirName     -- file-system tree
type DirNode    = (FilePath, DirName)  -- directory-path/dirname pair
-- TODO: make DirNode a record type, combine it with Item below

data Item = Folder [DirName] | File String deriving Show


-- High-level program logic: process args and perform the appropriate action

main :: IO ()
main = do
	args <- getArgs
	run args

run :: [String] -> IO ()
run args = do
	(run' args) `catch` errorAction
	where
		-- errorAction err :: IOException = showError (ioe_description err)
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

getOrElse Nothing b = b
getOrElse (Just a) _ = a

type Action = QueryPath -> DirTree -> IO ()
type LookupAction = (TreePath, DirTree) -> IO ()

actions = [
		("get",     actionGet,     Just "print the contents of a snippet or list"),
		("set",     actionSet,     Just "set the contents of a snippet (create or update)"),
		("copy",    actionCopy,    Just "copy the contents of a snippet to the clipboard (requires `pyperclip`)"),
		("all",     actionAll,     Just "list all snippets"),
		-- ("rm",      actionRemove,  Just "remove a snippet or list"),
		("which",   actionWhich,   Just "show the path to a snippet"),
		("--help",  actionHelp,    Nothing)
	]

parseArgs :: [String] -> (Action, QueryPath)
parseArgs args = explitAction `getOrElse` defaultAction
	where
		explitAction = fmap useRemainingArgs $ parseAction (maybeHead args)
		useRemainingArgs action = (action, tail args)
		defaultAction = (actionGet, args)

maybeHead :: [a] -> Maybe a
maybeHead [] = Nothing
maybeHead (x:xs) = Just x

parseAction :: Maybe String -> Maybe Action
parseAction Nothing = Just actionHelp
parseAction (Just x) = fmap getAction $ find matchingAction actions
	where
		getAction (_, action, _) = action
		getActionName (name, _, _) = name
		matchingAction = (== x) . getActionName

actionGet :: Action
actionGet = lookupAction $ \node -> getSnippetContents node >>= putStrLn . pp

actionCopy :: Action
actionCopy search = lookupAction actionCopy' search
	where
		actionCopy' :: LookupAction
		actionCopy' node = getSnippetContents node >>= (copyToClipboard search)

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

-- promptAndCreate :: Action
promptAndCreate args tree = do
	continue <- (confirm $ "Folder " ++ (joinPath args) ++ " does not exist. create it?")
	unless continue $ fail "Cancelled."
	actionNew args tree
	return ()

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

actionNew :: Action
actionNew args tree = do
	let path = addToRoot tree args
	createDirectoryIfMissing True path
	outLn [plain "Created: ", green (joinPath args)]

actionWhich :: Action
actionWhich = lookupAction (putStrLn . joinPath . fst)

actionRemove :: Action
actionRemove = lookupAction (doRemove . fst) where
	doRemove path = do
			let path' = joinPath path
			continue <- confirm $ "really delete" ++ path' ++ "?"
			when continue (putStrLn $ "TODO: remove" ++ path')

lookupAction :: LookupAction -> Action
lookupAction action search tree = success `getOrElse` (fail $ "Could not find " ++ (describeSearch search))
	where
		success = fmap action (resolvePath search tree)

actionAll :: Action
-- TODO: make `all` render the contents, truncated at first `n` chatacters of first line
actionAll [] tree = render $ showTree tree
actionAll args _ = fail "too many arguments for `all`"

justifyLeft :: Int -> String -> String
justifyLeft n str = str ++ replicate (max 0 (n - length str)) ' '

actionHelp :: Action
actionHelp args tree = putStrLn (unlines generalHelp)
	where
		generalHelp = describeActions ++ [""] ++ describeLists
		helpOn (name, _, (Just desc)) = [pad name ++ desc]
		helpOn (name, _, Nothing) = []
		pad name = "  " ++ justifyLeft 5 name ++ " : "
		describeActions = "Actions:" : (concat $ map helpOn actions)
		describeLists = "Your lists:" : [pp (Folder (map rootLabel (subForest tree)))]

-- action helpers / workers

addToRoot tree path = joinPath ([rootLabel tree] ++ path)

copyToClipboard search item = do
	output <- readProcess "pyperclip" ["--copy"] (up item)
	putStr output -- should be empty, but just in case?
	outLn [plain "Copied ", yellow (describeSearch search), plain " to your clipboard"]

describeSearch :: QueryPath -> String
describeSearch = joinPath

getValue :: TreePath -> DirTree -> IO Item
getValue path tree = success `getOrElse` (fail $ "Could not find " ++ (describeSearch path))
	where success = (fmap getSnippetContents) (resolvePath path tree)

getSnippetContents :: (TreePath, DirTree) -> IO Item
getSnippetContents match@(path, tree) =
	(doesDirectoryExist (joinPath path)) >>= \isDir ->
		if isDir
			then return $ Folder (map rootLabel (subForest tree))
			else do
				let fullPath = (joinPath path)
				-- putStrLn $ "reading file: " ++ fullPath
				contents <- readFile fullPath
				return $ File contents


-- tree traversal / searching

firstMatch :: [Maybe a] -> Maybe a
firstMatch items = join $ find isJust items

resolvePath :: TreePath -> DirTree -> Maybe (TreePath, DirTree)
resolvePath path tree = foldl resolveOne (Just ([rootLabel tree], tree)) path

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
		findInLevel :: DirName -> TreePath -> Forest DirName -> Maybe (TreePath, DirTree)
		findInLevel needle prefix nodes = fmap (returnTree prefix) found
			where
				found = find (((==) needle) . rootLabel) nodes

returnTree :: TreePath -> DirTree -> (TreePath, DirTree)
returnTree prefix node = (ncons prefix node, node)

-- path manipulation

ncons :: TreePath -> DirTree -> TreePath
ncons xs x = xs `pcons` (rootLabel x)

pcons :: TreePath -> String -> TreePath
pcons xs x = xs ++ [x]

-- Effectful tree-builder for file-system hierarchies

joinOne a b = joinPath [a,b]

fsTraverse :: FilePath -> IO DirTree
fsTraverse path = (lazyUnfoldTreeM fsTraverseStep) (path, path)

fsTraverseStep :: DirNode -> IO (DirName, [DirNode])
fsTraverseStep dnode@(path, name) = do
	children <- fsGetChildren (joinOne path name)
	return (name, children)


-- Helper to get traversable directory entries

fsGetChildren :: FilePath -> IO [DirNode]
fsGetChildren path = do
	contents <- getDirectoryContents path `Prelude.catch` const (return [])
	let visibles = sort . filter (`notElem` [".", ".."]) $ contents
	let isDir name = doesDirectoryExist $ joinOne path name
	-- print visibles
	return (map ((,) path) visibles)

lazyUnfoldTreeM :: (b -> IO (a, [b])) -> b -> IO (Tree a)
lazyUnfoldTreeM step seed = do
	(root, seeds) <- step seed
	children <-
		unsafeInterleaveIO $
		mapM (lazyUnfoldTreeM step) seeds
	return (Node root children)

-- Purely functional tree-to-string formatting

showTree :: Tree String -> Output
showTree t = concat $ map (showNode []) (subForest t)

showNode :: TreePath -> Tree String -> Output
showNode path node =
	nodeRep ++ childRep
	where
		nodeName = rootLabel node
		nodeRep  = appendLn [plain $ (replicate (2 * length path) ' ') ++ " - " ++ nodeName]
		childRep = concat $ map (showNode (path ++ [nodeName])) (subForest node)

-- TODO: coloourize nodes based on whether they are folders / files
-- coloredNode :: TreePath -> String -> TextAtom
-- coloredNode path name = (if isDir then yellow else plain) name
-- 	where
-- 		isDir = doesDirectoryExist (joinPath $ path ++ [name])



-- pretty print
pp :: Item -> String
pp (Folder []) = "(empty list)"
pp (Folder contents) = " - " ++ (intercalate "\n - " contents)
pp (File contents) = contents

-- ugly print
up :: Item -> String
up (Folder contents) = unlines contents
up (File contents) = contents


-- output formatting

#ifndef MINIMAL
-- Colored formatting depends on System.Console.ANSI, which
-- is NOT guaranteed to be available when running interactively (via runghc)
type TextAtom = (Maybe Term.Color, String)
color :: Term.Color -> a -> (Maybe Term.Color, a)
color = (,) . Just
plain = (,) Nothing
red = color Term.Red
blue = color Term.Blue
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
green = id
yellow = id

renderTextAtom :: TextAtom -> IO ()
renderTextAtom "\n" = putStrLn ""
renderTextAtom s = putStr s
#endif

type Output = [TextAtom]

render :: Output -> IO ()
render outputs = mapM_ renderTextAtom outputs

appendLn t = t ++ [plain "\n"]
outLn = render . appendLn
