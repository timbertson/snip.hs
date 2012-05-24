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
	tree <- fsTraverse "."
	-- putStr $ drawTree tree
	putStr (show $ levels tree)
	-- mapM_ traverseAndPrint ["."]

traverseAndPrint :: Path -> IO ()
traverseAndPrint path =
	putStr . showTree =<< fsTraverse root
	where
		root = if "/" `isPrefixOf` path then "" else "."

findForest :: Path -> Forest DirName -> DirName -> Maybe DirNode
findForest prefix nodes needle = fmap mkDirNode found
	where
		mkDirNode node = (prefix, rootLabel node)
		found = find (((==) needle) . rootLabel) nodes

stringy :: DirTree -> Tree String
stringy = fmap show

-- Effectful tree-builder for file-system hierarchies
dirsep = "/"
joinPath lst = concat $ intersperse dirsep lst
joinOne a b = joinPath [a,b]

fsTraverse :: Path -> IO DirTree
-- fsTraverse = curry (lazyUnfoldTreeM fsTraverseStep)
fsTraverse path = (lazyUnfoldTreeM fsTraverseStep) (path, ".")

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
	(dirs, files) <- partitionM isDir visibles
	print visibles
	return (map ((,) path) visibles)

-- PartitionM - apply Data.List.Partition for a predicate of type (M Bool)

partitionM :: Monad m => (a -> m Bool) -> [a] -> m ([a],[a])
partitionM p xs = do
	(f,g) <- pMHelper p xs
	return (f [], g [])
	where
		pMHelper :: Monad m => (a -> m Bool) -> [a] -> m ([a] -> [a],[a] -> [a])
		pMHelper p xs = foldM help (id,id) xs
			where
				help (f,g) x = do
				b <- p x
				return (if b then (f . (x:),g) else (f,g . (x:)))

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
