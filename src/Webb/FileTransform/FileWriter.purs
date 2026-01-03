module Webb.FileTransform.FileWriter where

import Prelude

import Effect.Aff.Class (class MonadAff)
import Webb.FileTransform.Internal.FileData (FileData)


{- Various methods for writing the modified FileData to disk. Prior to this, we were
  able to change the file's extension, directory, and so on. At this stage, we
  have to make a decision about _how_ we intend to make the write -- whether we are willing
  to overwrite or not is the primary concern; but also we want to _clear_ the target directory
  or not. Basically, we are trying to express _all_ thoughts we might have when seeking to 
  write the file -- not just the write itself, but any questions we might have before, during,
  and after performing the write. The entire mental model of that situation should be here,
  so that we don't have to reach for other modules -- this is a particular level of
  abstraction.
-}


newtype FileWriter = F Unit


fileExists :: forall m. MonadAff m => FileWriter -> FileData -> m Boolean
fileExists writer fd = pure true

dirExists :: forall m. MonadAff m => FileWriter -> FileData -> m Boolean
dirExists writer fd = pure true

-- Overwrite the file at the target location, creating the directory if needed.
overwrite :: forall m. MonadAff m => FileWriter -> FileData -> m Unit
overwrite writer fd = pure unit

-- Write the file, but fail if the directory doesn't exist or the file already exists
write :: forall m. MonadAff m => FileWriter -> FileData -> m Boolean
write writer fd = pure true