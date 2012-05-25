#!/usr/bin/runhaskell
{-

Directory-tree-printing example: slow version
Tom Moertel <tom@moertel.com>
2007-03-20

See http://blog.moertel.com/articles/tag/directory_tree_series
(Look for Part 3)

Compile:  ghc -O2 -o tlist-slow --make tlist-slow.hs

Usage: ./tlist-slow [directory...]

NOTE:  If you run this program on a large directory hierarchy,
       prepare to wait.  It takes 0.1 second to process each
       file-system node it encounters.    (See fsVisit, below.)

-}

module Main (main) where

import Control.Monad
import Control.Applicative ((<|>))
import Data.List
import Data.Tree
import Data.Maybe (isJust)
import System.FilePath
import System.Directory
import System.Environment
import System.IO

import System.IO.Unsafe


-- Some convenient type synonyms

type QueryPath  = [String]
type TreePath   = [DirName]
type Path       = String           -- path
type DirName    = String           -- directory-entry name
type DirNode    = (Path, DirName)  -- directory-path/dirname pair
type DirTree    = Tree DirName     -- file-system tree

data Item = Folder [DirName] | File String deriving Show

-- High-level program logic:  get args and print a tree for each

main :: IO ()
main = do
	args <- getArgs
	run args

run args = do
	let (action, query) = parseArgs args
	homedir <- getHomeDirectory
	let base = joinOne homedir ".snip"
	createDirectoryIfMissing True base
	tree <- fsTraverse base
	action query tree

getOrElse Nothing b = b
getOrElse (Just a) _ = a

type Action = (QueryPath -> DirTree -> IO ())

parseArgs :: [String] -> (Action, QueryPath)
parseArgs [] = (actionHelp, [])
parseArgs args@(x:xs)
	| x == "get"  = (actionGet, xs)
	| x == "new"  = (actionNew, xs)
	| x == "set"  = (actionSet, xs)
	| x == "echo" = (actionGet, xs)
	| x == "all" = (actionAll, xs)
	| otherwise = (actionGet, args)

actionGet :: Action
actionGet search tree = (getValue search tree) >>= (putStrLn . pp)

actionCopy :: Action
actionCopy search tree = (getValue search tree) >>= printAndCopy

actionSet :: Action
actionSet args tree = do
	let relpath = init args
	let val = last args
	let path = addToRoot tree relpath
	writeFile path val
	putStrLn $ "Got it. " ++ (joinPath relpath) ++ " is now: " ++ val

actionNew :: Action
actionNew args tree = do
	let path = addToRoot tree args
	createDirectory path
	putStrLn $ "Created: " ++ (joinPath args)

actionAll :: Action
actionAll [] tree = do
	putStr $ show tree
actionAll args _ = fail "too many arguments for `all`"

actionHelp :: Action
actionHelp args tree = putStrLn $ ("Your lists:" ++ (pp (Folder (map rootLabel (subForest tree)))))

addToRoot tree path = joinPath ([rootLabel tree] ++ path)


-- pretty print
pp :: Item -> String
pp (Folder []) = "(empty list)"
pp (Folder contents) = "\n - " ++ (intercalate "\n - " contents)
pp (File contents) = contents

-- ugly print
up :: Item -> String
up (Folder contents) = intercalate "\n" contents
up (File contents) = contents

printAndCopy item = do
	putStrLn (pp item)
	copyToClipboard (up item)

copyToClipboard s = putStrLn "TODO: copy stuff to clipboard!"

getValue :: TreePath -> DirTree -> IO Item
getValue path tree = success `getOrElse` (fail $ "Could not find " ++ (intercalate ">" path))
	where success = (fmap Main.getContents) (resolvePath path tree)

getContents :: (TreePath, DirTree) -> IO Item
getContents match@(path, tree) =
	(doesDirectoryExist (joinPath path)) >>= \isDir ->
		if isDir
			then return $ Folder (map rootLabel (subForest tree))
			else do
				let fullPath = (joinPath path)
				putStrLn $ "reading file: " ++ fullPath
				contents <- readFile fullPath
				return $ File contents

traverseAndPrint :: Path -> IO ()
traverseAndPrint path =
	putStr . showTree =<< fsTraverse root
	where
		root = if "/" `isPrefixOf` path then "" else "."

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

ncons :: TreePath -> DirTree -> TreePath
ncons xs x = xs `pcons` (rootLabel x)

pcons :: TreePath -> String -> TreePath
pcons xs x = xs ++ [x]

firstMatch :: [Maybe a] -> Maybe a
firstMatch items = join $ find isJust items


stringy :: DirTree -> Tree String
stringy = fmap show

-- Effectful tree-builder for file-system hierarchies
-- dirsep = "/"
-- joinPath lst = intercalate dirsep lst
joinOne a b = joinPath [a,b]

fsTraverse :: Path -> IO DirTree
-- fsTraverse = curry (lazyUnfoldTreeM fsTraverseStep)
fsTraverse path = (lazyUnfoldTreeM fsTraverseStep) (path, path)

fsTraverseStep :: DirNode -> IO (DirName, [DirNode])
fsTraverseStep dnode@(path, node) = do
	name <- fsVisit dnode
	children <- fsGetChildren (joinOne path node)
	return (name, children)

-- Helper to visit a directory node via an "expensive" operation

fsVisit :: DirNode -> IO DirName
fsVisit (_, name) = do
	return name


-- Helper to get traversable directory entries

fsGetChildren :: Path -> IO [DirNode]
fsGetChildren path = do
	contents <- getDirectoryContents path `catch` const (return [])
	let visibles = sort . filter (`notElem` [".", ".."]) $ contents
	let isDir name = doesDirectoryExist $ joinOne path name
	-- (dirs, files) <- partitionM isDir visibles
	-- print visibles
	return (map ((,) path) visibles)

-- PartitionM - apply Data.List.Partition for a predicate of type (M Bool)

-- partitionM :: Monad m => (a -> m Bool) -> [a] -> m ([a],[a])
-- partitionM p xs = do
-- 	(f,g) <- pMHelper p xs
-- 	return (f [], g [])
-- 	where
-- 		pMHelper :: Monad m => (a -> m Bool) -> [a] -> m ([a] -> [a],[a] -> [a])
-- 		pMHelper p xs = foldM help (id,id) xs
-- 			where
-- 				help (f,g) x = do
-- 				b <- p x
-- 				return (if b then (f . (x:),g) else (f,g . (x:)))

lazyUnfoldTreeM :: (b -> IO (a, [b])) -> b -> IO (Tree a)
lazyUnfoldTreeM step seed = do
	(root, seeds) <- step seed
	children <-
		unsafeInterleaveIO $
		mapM (lazyUnfoldTreeM step) seeds
	return (Node root children)

-- purely functional "finder" functions

-- find :: DirTree -> Maybe DirNode
-- find t = 

-- Purely functional tree-to-string formatting

showTree :: Tree String -> String
showTree t = unlines (showNode "" "" "" t)

showNode :: String -> String -> String -> Tree String -> [String]
showNode leader tie arm node =
	nodeRep : showChildren node (leader ++ extension)
	where
		nodeRep   = leader ++ arm ++ tie ++ rootLabel node
		extension = case arm of "" -> ""; "`" -> "    "; _   -> "|   "

showChildren :: Tree String -> String -> [String]
showChildren node leader =
	let
		children = subForest node
		arms = replicate (length children - 1) "|" ++ ["`"]
	in concat (zipWith (showNode leader "-- ") arms children)
