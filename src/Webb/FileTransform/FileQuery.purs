module Webb.FileTransform.FileQuery where

import Prelude

import Data.Foldable as Fold
import Data.Newtype (wrap)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import Webb.AffList (AffList, runYieldToList)
import Webb.AffList.Monad.Yield (yield)
import Webb.Directory.Data.Absolute as Abs
import Webb.File as File
import Webb.FileTransform.Internal.FileData (FileData)
import Webb.FileTransform.PathQuery as PathQuery
import Webb.State.Prelude (newShowRef)


{- Implement file querying. Since all files together may be large, we process only one file at a time -- using an AffList. This requires us to present a reasonable data type to represent the file, so that we can transform it.
-}

newtype FileQuery = Q Unit

newQuery :: forall m. MonadEffect m => m FileQuery
newQuery = do pure $ Q unit

-- Expose the files in a directory by the extension, fetching each file one at a time,
-- only on request, for processing.
queryByExt :: forall m. MonadAff m => FileQuery -> Abs.AbsPath ->  String -> AffList (FileData)
queryByExt _self dir ext = runYieldToList do
  p <- PathQuery.newQuery
  paths <- PathQuery.queryExt p dir ext
  Fold.for_ paths \path -> do 
    file <- File.newFile path
    text <- File.readAllText file
    pathRef <- newShowRef path
    stringRef <- newShowRef text
    
    let fileData = wrap
          { source: path
          , path: pathRef
          , string: stringRef
          } :: FileData

    yield fileData