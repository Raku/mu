module SHA1-0.0.1;

inline Haskell => '
    import qualified SHA1
    sha1 :: String -> String
    sha1 = SHA1.sha1
';

&sha1 is export;

=kwid

= NAME

SHA1 - SHA1 Digests

= AUTHOR

Brian Ingerson <ingy@cpan.org>

=cut
