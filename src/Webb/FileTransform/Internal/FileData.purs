module Webb.FileTransform.Internal.FileData where

import Prelude
import Webb.State.Prelude

import Effect.Class (class MonadEffect)
import Webb.Directory.Data.Absolute as Abs
import Webb.FileTransform.Data.Path as Path


{- Opaque handle on the file data, so that we can safely transform it without knowing its 
internal representation.

-}


newtype FileData = F
  { source :: Abs.AbsPath
  , path :: ShowRef Abs.AbsPath
  , string :: ShowRef String
  }
  
source :: forall m. MonadEffect m => FileData -> m Abs.AbsolutePath
source (F s) = pure s.source

-- Read the full path
path :: forall m. MonadEffect m => FileData -> m Abs.AbsolutePath
path (F s) = aread s.path

setPath :: forall m. MonadEffect m => FileData -> Abs.AbsPath -> m Unit
setPath (F s) p = s.path := p

-- Return the file extension _without_ the dot.
extname :: forall m. MonadEffect m => FileData -> m String
extname (F s) = do Path.extname <: s.path
  
-- Set the extension of the path. This enables us to modify the file that we're writing to.
setExtname :: forall m. MonadEffect m => FileData -> String -> m Unit
setExtname (F s) name = do Path.setExtname name :> s.path
  
-- Return the directory path for this file.
dir :: forall m. MonadEffect m => FileData -> m Abs.AbsPath
dir (F s) = do Path.dirpath <: s.path
  
-- Set the directory path for this file. This lets us move the file "informally" by
-- setting data and calculating path changes, without affecting the file name.
-- We change the metadata that represents the file -- and then we write it.
setDir :: forall m. MonadEffect m => FileData -> Abs.AbsPath -> m Unit
setDir (F s) p = do Path.setDirpath p :> s.path
  
text :: forall m. MonadEffect m => FileData -> m String
text (F s) = do aread s.string

setText :: forall m. MonadEffect m => FileData -> String -> m Unit
setText (F s) str = do s.string := str