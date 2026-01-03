module Webb.FileTransform.Data.Path where

import Prelude

import Webb.String as String
import Node.Path as Path
import Webb.Directory.Data.Absolute ((++))
import Webb.Directory.Data.Absolute as Abs


type TPath = Unit

-- extensions need notinclude the "." at the front. So we remove it before comparing.
normalizeExt :: String -> String
normalizeExt str = String.dropWhile (codepointIs ".") str
  where
  codepointIs s cp = (String.fromCodePointArray [cp] == s)

-- Read the extension, without the ending "."
extname :: Abs.AbsPath -> String
extname path = let
  str = Abs.unwrap path
  name = Path.extname str
  in normalizeExt name
  
setExtname :: String -> Abs.AbsPath -> Abs.AbsPath
setExtname str path = let 
  path' = withoutExt
  str' = extString
  in concat path' str'
  
  where
  withoutExt :: Abs.AbsPath
  withoutExt = let 
    dropCount = String.length (extname path)
    in Abs.modify (String.dropEnd dropCount) path
    
  extString :: String
  extString = normalizeExt str
  
  hadExt :: Boolean
  hadExt = path /= withoutExt
  
  concat :: Abs.AbsPath -> String -> Abs.AbsPath
  concat p ext = 
    if hadExt then  
      -- No extension existed. We need to add the "."
      Abs.modify (_ <> "." <> ext) p
    else 
      -- An extension existed. We dropped everything but the ".", so we can append directly
      Abs.modify (_ <> ext) p
      
-- Return the basename of the path. This is no longer an absolute path, but a fragment.
basename :: Abs.AbsPath -> String
basename path = let
  str = Abs.unwrap path
  in Path.basename str
  
-- Replace the basename of the path entirely.
setBasename :: String -> Abs.AbsPath -> Abs.AbsPath
setBasename str path = let 
  dir = dirpath path
  in dir ++ str
  
dirpath :: Abs.AbsPath -> Abs.AbsPath
dirpath path = let 
  str = Abs.unwrap path
  name = Path.dirname str
  in Abs.new [] name
  
setDirpath :: Abs.AbsPath -> Abs.AbsPath -> Abs.AbsPath
setDirpath dir full = let  
  base = basename full
  in dir ++ base


