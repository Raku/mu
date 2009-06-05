say "1..4";

{
    # :($ok1 is ref,$ok2 is ref)
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
}.();

{
    my $var;
    # :($ref is ref)
    my $code = sub ($ref) {
        $ref = "ok 3";
    }
    
    my $sig = ::Signature.new();
    $sig.positionals = ::Array.new;
    
    my $param1 = ::RefParam.new;
    $param1.name = '$ref';
    $sig.positionals.push($param1.FETCH);

    my $code2 = ::Code.new(:outer($code.outer),:mold($code.mold),:signature((|$sig)));
    $code2.($var);
    say $var;
}.();

# :($readonly)
{
    my $sig = ::Signature.new();
    $sig.positionals = ::Array.new;
    my $code = sub ($readonly) {
        $readonly = 1;
    }
    CATCH {
        say "ok 4 # can't assign to a readonly var";
    }

    my $param = ::ReadonlyParam.new;
    $param.name = '$readonly';
    $sig.positionals.push($param.FETCH);
#
    my $code2 = ::Code.new(:outer($code.outer),:mold($code.mold),:signature((|$sig)));
    my $foo = 1;
    $code2.($foo);
}.();

# :($default = "ok 4"))
{
    my $sig = ::Signature.new();
    $sig.positionals = ::Array.new;
    my $code = sub ($default) {
        say $default;
    }

    my $param = ::RefParam.new;
    $param.name = '$default';
    $param.default_value = sub {"ok 5"}
    $sig.positionals.push($param.FETCH);
#
    my $code2 = ::Code.new(:outer($code.outer),:mold($code.mold),:signature((|$sig)));
    my $foo = 1;
    $code2.();
}.();
