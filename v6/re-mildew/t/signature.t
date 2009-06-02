say "1..1";
my $code = sub ($ok1,$ok2) {
    say "$ok1\n$ok2";
}

my $sig = ::Signature.new();
$sig.positionals = ::Array.new;

my $param1 = ::RefParam.new;
$param1.name = '$ok1';

my $param2 = ::ReadonlyParam.new;
$param2.name = '$ok2';

$sig.positionals.push($param1.FETCH);
$sig.positionals.push($param2.FETCH);
my $code2 = ::Code.new(:outer($code.outer),:mold($code.mold),:signature((|$sig)));
$code2.("ok 1","ok 2");

{
    my $sig = ::Signature.new();
    $sig.positionals = ::Array.new;
    my $code = sub ($readonly) {
        $readonly = 1;
    }
    CATCH {
        say "ok 3 # can't assign to a readonly var";
    }

    my $param = ::ReadonlyParam.new;
    $param.name = '$readonly';
    $sig.positionals.push($param.FETCH);
#
    my $code2 = ::Code.new(:outer($code.outer),:mold($code.mold),:signature((|$sig)));
    my $foo = 1;
    $code2.($foo);
}.();
