say "1..6";

{
    # :($ok1 is ref,$ok2)
    my $code = sub ($ok1,$ok2) {
        say "$ok1\n$ok2";
    }
    
    my $sig = :($ok1 is ref,$ok2);

    my $code2 = ::Code.new(:outer($code.outer),:mold($code.mold),:signature((|$sig)));
    $code2("ok 1","ok 2");
}

{
    my $var;
    my $code = sub ($ref) {
        $ref = "ok 3";
    }
    
    my $sig = :($ref is ref);

    my $code2 = ::Code.new(:outer($code.outer),:mold($code.mold),:signature((|$sig)));
    $code2($var);
    say $var;
}

{
    my $code = sub ($readonly) {
        $readonly = 1;
        say "not ok 4 # can assign to a readonly var";
    }
    CATCH {
        say "ok 4 # can't assign to a readonly var";
    }

    my $sig = :($readonly);
 
    my $code2 = ::Code.new(:outer($code.outer),:mold($code.mold),:signature((|$sig)));
    my $foo = 1;
    $code2($foo);
}

# :($default is ref = "ok 5"))
{
    my $sig = ::Signature.new();
    my $code = sub ($default) {
        say $default;
    }

    my $param = ::RefParam.new;
    $param.variable = '$default';
    $param.default_value = sub {"ok 5"}
    $sig.params.push($param.FETCH);
#
    my $code2 = ::Code.new(:outer($code.outer),:mold($code.mold),:signature((|$sig)));
    my $foo = 1;
    $code2();
}
# :($default = "ok 6"))
{
    my $sig = ::Signature.new();
    my $code = sub ($default) {
        say $default;
    }

    my $param = ::ReadonlyParam.new;
    $param.variable = '$default';
    $param.default_value = sub {"ok 6"}
    $sig.params.push($param.FETCH);
#
    my $code2 = ::Code.new(:outer($code.outer),:mold($code.mold),:signature((|$sig)));
    my $foo = 1;
    $code2();
}
