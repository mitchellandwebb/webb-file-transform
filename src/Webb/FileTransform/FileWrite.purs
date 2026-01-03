module Webb.FileTransform.FileWrite where

import Prelude


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
