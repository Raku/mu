{-# OPTIONS -fglasgow-exts -cpp #-}

{-
    External call utilities.

    To the Sea, to the Sea! The white gulls are crying,
    The wind is blowing, and the white foam is flying.
    West, west away, the round sun is falling.
    Grey ship, grey ship, do you hear them calling?
-}

module External (
    module External.C,
    module External.Haskell,
) where
import External.C
import External.Haskell
