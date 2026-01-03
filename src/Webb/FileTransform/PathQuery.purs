module Webb.FileTransform.PathQuery where

import Prelude
import Webb.State.Prelude

import Data.String as String
import Data.Traversable as Traverse
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import Node.Path as Path
import Webb.Array as Array
import Webb.Directory.Data.Absolute (AbsolutePath, AbsPath)
import Webb.Directory.Data.Absolute as Abs
import Webb.Directory.Visitor as Visit
import Webb.Monad.Prelude ((&&=), (||=))
import Webb.Set as Set

{- Query for specific files. Includes the ability to query for specific file
  paths as well.
-}

newtype PathQuery = Q Unit

newQuery :: forall m. MonadEffect m => m PathQuery
newQuery = pure $ Q unit

-- We query all paths. To avoid recursive symlinks (as best we can), we store the 
-- absolute paths that we have already seen, and we AVOID going upward or sideways 
-- in the paths. We want to pretend this is a tree, not a graph.
queryAll :: forall m. MonadAff m => PathQuery -> AbsolutePath -> m (Array AbsolutePath)
queryAll self path = do queryAllFilter self path (\_ -> true)

-- Query for the extension. We normalize the extensions for the '.' before comparing
-- them to each other.
queryExt :: forall m. MonadAff m => PathQuery -> AbsolutePath -> String -> m (Array AbsolutePath)
queryExt self path ext = do queryAllFilter self path hasExt
  where
  hasExt file = do 
    let string = Abs.unwrap file
    extNormal (Path.extname string) == extNormal ext
    
-- extensions need not include the "." at the front. So we remove it before comparing.
extNormal :: String -> String
extNormal str = String.dropWhile (codepointIs ".") str
  where
  codepointIs s cp = (String.fromCodePointArray [cp] == s)

queryAllFilter :: forall m. MonadAff m => 
  PathQuery -> AbsolutePath -> (AbsolutePath -> Boolean) -> m (Array AbsolutePath)
queryAllFilter _ path f = do 
  visited <- newShowRef (Set.empty)
  files <- newShowRef []
  recurse path visited files
  aread files
  where 
  recurse :: AbsPath -> ShowRef (Set.Set AbsolutePath) -> ShowRef (Array AbsPath) -> m Unit
  recurse dir visited files = do 
    ifM wasVisited (do 
      pure unit 
    ) (do 
      markVisited
      addFiles
      cs <- getChildDirs
      Traverse.for_ cs \child -> do
        recurse child visited files
    )
    where 
    wasVisited = Set.member dir <: visited
    markVisited = Set.insert dir :> visited
    addFiles = do 
      v <- Visit.newVisitor dir
      newFiles <- Visit.files v
      (_ <> newFiles) :> files

    getChildDirs = do 
      v <- Visit.newVisitor dir
      cs <- Visit.dirs v
      Array.filterA (\c -> isChild c &&= notVisited c &&= isMatch c) cs
      
    -- To be a child, we need to be deeper than the directory.
    isChild c = do 
      pure $ Abs.depth c > Abs.depth dir
      
    notVisited c = do
      (not <<< Set.member c) <: visited
      
    isMatch c = do 
      pure $ f c
