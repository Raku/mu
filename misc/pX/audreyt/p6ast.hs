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

package Main;

sub f (constant $x = 1) { ... }

f(); # $x retains last value

submethod BUILD (has $x) { ... }
submethod BUILD ($.x) { ... }
$Main::x


positional named named_slurpy

method f (Moose $self where {...}: ...)


(@a [$x, $y, *@rest [*, *, *, $z]]  ) := [1,2,3]


sub f (:v($verbose), :$verbose, :$v) {
    say $verbose;
}

f(v=>1);
f(verbose=>1);



Any $self (\)

sub f ($x, $x, $x) { }

method f ($x, $y) {}

data Param = MkParam
    { paramVariable     :: Identifier
    , paramTypes        :: [Type]       -- Static pieces of inferencer-food
    , paramConstraints  :: [Code]       -- Dynamic pieces of runtime-mood
    , paramUnpacking    :: Maybe Signature
    , paramDefault      :: Maybe Expression
    , paramLabel        :: Identifier
    }

data Variable
    = VarLexical
        { varName     :: Identifier
        , callerCount :: Int
        , outerCount  :: Int
        }
    | VarDynamic
        { varName     :: Identifier
        , packageName :: [Identifier]
        }
    | VarMagick Magick

data Expression
    = ExpVariable Variable
    | ExpValue    Value
    | ExpBind
    | ExpAssign
    | ExpCall
    | ExpApply
    -- The Army Of Decompile {-
    | ExpCond
    | ExpGoto
    | ExpCall
    | ExpWhile
    | ExpBlahBlahBlah
    | ExpDeref
    -- -}
    --
--if 1 { 2 } else { 3 }
&statement_control:<if>.(1,2,3)

%h<1>



    &statement_control<if>.wrap(...)
    &statement_control<if> <== ...


-- Anything but $?SELF -- See S02
data Magick
    | MagickClass
    | MagickOS
    | MagickVersion
    | MagickMushroom

data Signature
    = MkSignatureMethSingle
        { invocant                  :: Param
        , requiredPositionalCount   :: Int
        , requiredNames             :: Set Identifier
        , positionalList            :: [Param]
        , namedSet                  :: Map Identifier Param
        , slurpyScalarList          :: [Param]
        , slurpyArray               :: Maybe Param
        , slurpyHash                :: Maybe Param
        , slurpyCode                :: Maybe Param
        }
    | MkSignatureSubSingle
        { requiredPositionalCount   :: Int
        , requiredNames             :: Set Identifier
        , positionalList            :: [Param]
        , namedSet                  :: Map Identifier Param
        , slurpyScalarList          :: [Param]
        , slurpyArray               :: Maybe Param
        , slurpyHash                :: Maybe Param
        , slurpyCode                :: Maybe Param
        }

data Code
    = MkSingle SingleCode
    | MkMulti (Set MultiVariant)

data SingleCode = MkSingleCode
    { signature  :: Signature
    , body       :: [Statement]
    , pad        :: Map Identifier PadEntry
    , attributes :: Map Identifier Value
    }
    
data MultiVariant = MkMultiVariant 
    { semicolonOffsets          :: IntSet
    , theThingYouActuallyCall   :: SingleCode
    }


my multi g () { ... }
{
    my multi f ($x) { ... }
    my multi f ($x, $y) { ... }
    &g.variants.push(&f);
}

(IntSet, Code)) -- Map from full long names to Code
                     -- {2,3,4}
                     -- ^ The cursors, think of the ncurses (n. slang)!
                     -- $pun, $gun; $way; $too; $far

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

