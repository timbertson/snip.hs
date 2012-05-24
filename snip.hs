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
import Data.List
import Data.Tree
import System.Directory
import System.Environment
import System.Posix (usleep)

import System.IO.Unsafe


-- Some convenient type synonyms

type Path       = String           -- path
type DirName    = String           -- directory-entry name
type DirNode    = (Path, DirName)  -- directory-path/dirname pair
type DirTree    = Tree DirName     -- file-system tree


-- High-level program logic:  get args and print a tree for each

main :: IO ()
main = do
	args <- getArgs
	mapM_ traverseAndPrint (if null args then ["."] else args)

traverseAndPrint :: Path -> IO ()
traverseAndPrint path =
	putStr . showTree =<< fsTraverse root path
	where
		root = if "/" `isPrefixOf` path then "" else "."


-- Effectful tree-builder for file-system hierarchies

fsTraverse :: Path -> DirName -> IO DirTree
fsTraverse = curry (lazyUnfoldTreeM fsTraverseStep)

fsTraverseStep :: DirNode -> IO (DirName, [DirNode])
fsTraverseStep dnode@(path, node) = do
	name <- fsVisit dnode
	children <- fsGetChildren (path ++ "/" ++ node)
	return (name, children)


-- Helper to visit a directory node via an "expensive" operation

fsVisit :: DirNode -> IO DirName
fsVisit (_, name) = do
	-- usleep 100000
	return name


-- Helper to get traversable directory entries

fsGetChildren :: Path -> IO [DirNode]
fsGetChildren path = do
	contents <- getDirectoryContents path `catch` const (return [])
	let visibles = sort . filter (`notElem` [".", ".."]) $ contents
	return (map ((,) path) visibles)


lazyUnfoldTreeM :: (b -> IO (a, [b])) -> b -> IO (Tree a)
lazyUnfoldTreeM step seed = do
	(root, seeds) <- step seed
	children <-
		unsafeInterleaveIO $
		mapM (lazyUnfoldTreeM step) seeds
	return (Node root children)

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
