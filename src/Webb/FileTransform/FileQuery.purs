module Webb.FileTransform.FileQuery where

import Prelude


{- Implement file querying. Since all files together may be large, we process only one file at a time -- using an AffList. This requires us to present a reasonable data type to represent the file, so that we can transform it.
-}

newtype FileQuery = Q Unit