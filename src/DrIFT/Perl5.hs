{-# OPTIONS_GHC -fglasgow-exts -funbox-strict-fields #-}

module DrIFT.Perl5 where
import Data.List (intersperse)

class (Show a) => Perl5 a where
    showPerl5 :: a -> String
    showPerl5 x = show (show x)

-- XXX - overlapping instances?

instance Perl5 Int where
    showPerl5 = show

instance Perl5 String where
    showPerl5 = show

instance Perl5 Bool where
    showPerl5 True = "1"
    showPerl5 False = "0"

instance (Perl5 a) => Perl5 (Maybe a) where
    showPerl5 (Just x) = showPerl5 x
    showPerl5 Nothing = "(undef)"

instance (Perl5 a) => Perl5 [a] where
    showPerl5 xs = "[" ++ (concat $ intersperse ", " (map showPerl5 xs)) ++ "]"

instance (Perl5 a, Perl5 b) => Perl5 (a, b) where
    showPerl5 (x, y) = "[" ++ showPerl5 x ++ " => " ++ showPerl5 y ++ "]"

