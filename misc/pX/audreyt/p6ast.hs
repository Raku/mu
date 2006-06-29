COMPUNIT = module Main {
    sub MAIN {
        say 1;
    }
}

my $x ::= 1;

data Value
    = VConstant  Unboxed     -- 1
    | VObject    Object      -- Foo.new(...)
    | VForeign   Foreign     -- perl5:DBI.connect()
    | VCode      Code        -- {...}
    | VType      Type        -- ::CGI
    | VCapture   Capture     -- \(...)
    | VSignature Signature   -- :(...)

multi ( Int $x, What $y ; Str $z ; Str $w )
multi ( Int $x, Num $y  ; Str $z ; Str $w )

data Signature
    = MkSignatureMeth
        { invocant :: Expression
        , 
        }
    | MkSignatureSub
        {
        }

data Code
    = MkCode
        { signature  :: Signature
        , body       :: [Statement]
        , pad        :: Map Identifier PadEntry
        , attributes :: Map Identifier Value
        }
    | MkMulti (Map Signature Code) -- Map from full long names to Code

multi f (Int $x; Int $y) {...}
multi f (Int $x) {...}

data PadEntry
    = MkEntry StorageType (TVar Value)

data Unboxed
    = UInt Int      -- 3
    | UNum Num      -- 4.2
    | UStr Str      -- 'aloha'
    | UBit Bit      -- True

type ObjectId = Unboxed

data Object = MkObject
    { objectId :: ObjectId
    , metaId   :: ObjectId
    , slots    :: Map Identifier Value
    }

data Capture
    = MkCaptureMeth
        { invocant :: Expression
        , argstack :: [Arglist]
        }
    | MkCaptureSub
        { argstack :: [Arglist]
        }

data Capture = MkCapture
    { invocant :: Maybe Expression
    , argstack :: [Arglist]
    }

data Arglist = MkArglist
    { positional :: [Exp]
    , named      :: Map Str [Expression]
    }

