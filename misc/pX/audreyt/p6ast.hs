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
    | VarMagic Magic

data Expression
    = ExpVariable Variable
    | ExpValue    Value
    | ExpDeref    Variable
    | ExpBind     Expression Expression
    | ExpAssign   Expression Expression
    | ExpControl  Control

data Control
    = ContCall                           -- lookup a routine, call it
    | ContApply                          -- apply a Code immediately
    | ContCond                           -- statement_control:<if>
    | ContGoto                           -- statement_control:<goto>
    | ContWhile                          -- statement_control:<while>
    | ContForeign                        -- statement_control:<mycontrol>

data Statement = MkStatment
    { label      :: Identifier
    , pragmas    :: Map Identifier Value
    , expression :: Expression
    }




--if 1 { 2 } else { 3 }
&statement_control:<if>.(1,2,3)

%h<1>



    &statement_control<if>.wrap(...)
    &statement_control<if> <== ...


data Magic
    = MagicOS            -- $?OS        Which os am I compiled for?
    | MagicOSVer         -- $?OSVER     Which os version am I compiled for?
    | MagicPerlVer       -- $?PERLVER   Which Perl version am I compiled for?
    | MagicFile          -- $?FILE      Which file am I in?
    | MagicLine          -- $?LINE      Which line am I at?
    | MagicScalarPackage -- $?PACKAGE   Which package am I in?
    | MagicArrayPackages -- @?PACKAGE   Which packages am I in?
    | MagicScalarModule  -- $?MODULE    Which module am I in?
    | MagicArrayModules  -- @?MODULE    Which modules am I in?
    | MagicScalarClass   -- $?CLASS     Which class am I in? (as variable)
    | MagicArrayClasses  -- @?CLASS     Which classes am I in?
    | MagicScalarRole    -- $?ROLE      Which role am I in? (as variable)
    | MagicArrayRoles    -- @?ROLE      Which roles am I in?
    | MagicScalarGrammar -- $?GRAMMAR   Which grammar am I in?
    | MagicArrayGrammars -- @?GRAMMAR   Which grammars am I in?
    | MagicParser        -- $?PARSER    Which Perl grammar was used to parse this statement?
    | MagicScalarRoutine -- &?ROUTINE   Which routine am I in?
    | MagicArrayRoutines -- @?ROUTINE   Which routines am I in?
    | MagicScalarBlock   -- &?BLOCK     Which block am I in?
    | MagicArrayBlocks   -- @?BLOCK     Which blocks am I in?


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


$x = 'moose';
$x = $x ~ 'elk';

class ClassyExample does Scalar {
    STORE
    FETCH
}

    scalar_store(castMut(Object(meta=ClassyExample,slots={})), castV(Str('moose')))

my $y is ClassyExample;
$y = "moose"; # now with style!


data Val
    = Pure VPure
    | Mut VMut
    | IO VIO


data VIO
    = VISocket Socket

data VMut
    = VMScalar (TVar Val)

data VPure
    = VPInt Int

instance ScalarClass a where
    doScalarFetch :: a -> Eval Val
    doScalarStore :: a -> Eval Val

$x = @a;

scalar_fetch :: Val -> Eval Val
scalar_fetch v@Pure{} = return v
scalar_fetch v@(Mut m) = case m of
    VMScalar s  -> doScalarFetch s
    VMArray{}   -> v
    _           -> fail ""

instance ScalarClass (TVar Val) where
    -- vivify?
    doScalarFetch t = do
        lv <- asks envLValue
        case lv of
            RValue -> liftSTM (readTVar t)
            LValue typ -> do
                rv <- readTVar t
                case rv of
                    Pure VPUndef -> do
                        writeTVar t =<< fmap Mut (newLValue typ)
                    _ -> return rv

# This is a mutable capture and totally offtopic
my $x = Match(blah => 2);
$x[1] = 2;

