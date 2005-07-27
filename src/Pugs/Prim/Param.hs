{-# OPTIONS_GHC -fglasgow-exts #-}

module Pugs.Prim.Param (
    foldParam
) where
import Pugs.AST

doFoldParam :: String -> String -> [Param] -> [Param]
doFoldParam cxt [] []       = [(buildParam cxt "" "$?1" (Val VUndef)) { isLValue = False }]
doFoldParam cxt [] (p:ps)   = ((buildParam cxt "" (strInc $ paramName p) (Val VUndef)) { isLValue = False }:p:ps)
doFoldParam cxt (s:name) ps = ((buildParam cxt [s] name (Val VUndef)) { isLValue = False } : ps)

foldParam :: String -> Params -> Params
foldParam "Named" = \ps -> (
    (buildParam "Hash" "*" "@?0" (Val VUndef)):
    (buildParam "Hash" "*" "%?0" (Val VUndef)):ps)
foldParam "List"    = doFoldParam "Any" "*@?1"
foldParam ('r':'w':'!':"List") = \ps -> ((buildParam "List" "" "@?0" (Val VUndef)) { isLValue = True }:ps)
foldParam ('r':'w':'!':str) = \ps -> ((buildParam str "" "$?1" (Val VUndef)) { isLValue = True }:ps)
foldParam ""        = id
foldParam ('?':str)
    | ('r':'w':'!':typ) <- str
    = \ps -> ((buildParam typ "?" "$?1" (Val VUndef)) { isLValue = True }:ps)
    | (('r':'w':'!':typ), "=$_") <- break (== '=') str
    = \ps -> ((buildParam typ "?" "$?1" (Var "$_")) { isLValue = True }:ps)
    | (typ, "=$_") <- break (== '=') str
    = \ps -> ((buildParam typ "?" "$?1" (Var "$_")) { isLValue = False }:ps)
    | (typ, ('=':def)) <- break (== '=') str
    = let readVal "Num" = Val . VNum . read
          readVal "Int" = Val . VInt . read
          readVal "Str" = Val . VStr . read
          readVal x     = error $ "Unknown type: " ++ x
      in \ps -> ((buildParam typ "?" "$?1" (readVal typ def)) { isLValue = False }:ps)
    | otherwise
    = \ps -> (buildParam str "?" "$?1" (Val VUndef):ps)
foldParam ('~':str) = \ps -> (((buildParam str "" "$?1" (Val VUndef)) { isLValue = False }) { isLazy = True }:ps)
foldParam x         = doFoldParam x []
