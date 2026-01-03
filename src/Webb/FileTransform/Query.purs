module Webb.FileTransform.Query where

import Prelude
import Webb.State.Prelude

import Data.Traversable as Traverse
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import Webb.Array as Array
import Webb.Directory.Data.Absolute (AbsolutePath, AbsPath)
import Webb.Directory.Data.Absolute as Abs
import Webb.Directory.Visitor as Visit
import Webb.Set as Set
import Webb.Monad.Prelude ((&&=), (||=))

{- Query for specific files. Includes the ability to query for specific file
  paths as well.
-}

newtype Query = Q Unit

newQuery :: forall m. MonadEffect m => m Query
newQuery = pure $ Q unit

-- We query all paths. To avoid recursive symlinks (as best we can), we store the 
-- absolute paths that we have already seen, and we AVOID going upward or sideways 
-- in the paths. We want to pretend this is a tree, not a graph.
queryAll :: forall m. MonadAff m => Query -> AbsolutePath -> m (Array AbsolutePath)
queryAll self path = do queryAllFilter self path (\_ -> true)

queryAllFilter :: forall m. MonadAff m => 
  Query -> AbsolutePath -> (AbsolutePath -> Boolean) -> m (Array AbsolutePath)
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
