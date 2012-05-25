#!/usr/bin/runhaskell
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

import Control.Monad
import Control.Applicative ((<|>))
import Data.List
import Data.Tree
import Data.Maybe (isJust)
import System.FilePath
import System.Directory
import System.Environment
import System.IO
import System.Process (readProcess)

import System.IO.Unsafe


-- Some convenient type synonyms

type QueryPath  = [String]
type TreePath   = [DirName]
type Path       = String           -- path
type DirName    = String           -- directory-entry name
type DirNode    = (Path, DirName)  -- directory-path/dirname pair
type DirTree    = Tree DirName     -- file-system tree

data Item = Folder [DirName] | File String deriving Show

-- High-level program logic: process args and perform the appropriate action

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
type LookupAction = (TreePath, DirTree) -> IO ()

parseArgs :: [String] -> (Action, QueryPath)
parseArgs [] = (actionHelp, [])
parseArgs args@(x:xs)
	| x == "get"   = (lookupAction actionGet, xs)
	| x == "new"   = (actionNew, xs)
	| x == "set"   = (actionSet, xs)
	| x == "copy"  = (actionCopy, xs)
	| x == "all"   = (actionAll, xs)
	| x == "rm"    = (lookupAction actionRemove, xs)
	| x == "which" = (lookupAction actionWhich, xs)
	| otherwise    = (lookupAction actionGet, args)

actionGet :: LookupAction
actionGet node = getSnippetContents node >>= putStrLn . pp

actionCopy :: Action
actionCopy search tree = (getValue search tree) >>= copyToClipboard search

actionSet :: Action
actionSet args tree = do
	let relpath = init args
	val <- getInput $ last args
	let path = addToRoot tree relpath
	writeFile path val
	putStrLn $ "Got it. " ++ (joinPath relpath) ++ " is now: " ++ val

getInput "-" = do
	putStrLn "(reading from stdin, ctrl+d to finish)"
	getContents
getInput val = return val

actionNew :: Action
actionNew args tree = do
	let path = addToRoot tree args
	createDirectory path
	putStrLn $ "Created: " ++ (joinPath args)

actionWhich :: LookupAction
actionWhich = putStrLn . joinPath . fst

actionRemove :: LookupAction
actionRemove (path, _) = do
			let path' = joinPath path
			putStrLn $ "really delete" ++ path' ++ "? [Y/n]"
			answer <- getLine
			when (answer `elem` ["","y","Y"]) (putStrLn $ "TODO: remove" ++ path')

lookupAction :: LookupAction -> Action
lookupAction action search tree = success `getOrElse` (fail $ "Could not find " ++ (describeSearch search))
	where
		success = fmap action (resolvePath search tree)

actionAll :: Action
actionAll [] tree = do
	putStr $ showTree tree
actionAll args _ = fail "too many arguments for `all`"

actionHelp :: Action
actionHelp args tree = putStrLn $ ("Your lists:\n" ++ (pp (Folder (map rootLabel (subForest tree)))))

-- action helpers / workers

addToRoot tree path = joinPath ([rootLabel tree] ++ path)

copyToClipboard search item = do
	output <- readProcess "pyperclip" ["--copy"] (up item)
	putStr output -- should be empty, but just in case?
	putStrLn $ "Copied " ++ (describeSearch search) ++ " to your clipboard."

describeSearch :: QueryPath -> String
describeSearch = intercalate ">"

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


-- pretty print
pp :: Item -> String
pp (Folder []) = "(empty list)"
pp (Folder contents) = " - " ++ (intercalate "\n - " contents)
pp (File contents) = contents

-- ugly print
up :: Item -> String
up (Folder contents) = intercalate "\n" contents
up (File contents) = contents


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

fsTraverse :: Path -> IO DirTree
fsTraverse path = (lazyUnfoldTreeM fsTraverseStep) (path, path)

fsTraverseStep :: DirNode -> IO (DirName, [DirNode])
fsTraverseStep dnode@(path, name) = do
	children <- fsGetChildren (joinOne path name)
	return (name, children)


-- Helper to get traversable directory entries

fsGetChildren :: Path -> IO [DirNode]
fsGetChildren path = do
	contents <- getDirectoryContents path `catch` const (return [])
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

showTree :: Tree String -> String
showTree t = unlines (concat $ map (showNode "") (subForest t))

showNode :: String -> Tree String -> [String]
showNode leader node =
	nodeRep : childRep
	where
		nodeRep  = leader ++ " - " ++ rootLabel node
		childRep = concat $ map (showNode (leader ++ "  ")) (subForest node)
