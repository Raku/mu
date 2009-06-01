role Signature {
    has @.positionals;
    method ACCEPTS(\$capture,$scope) {
        say "ACCEPTS";
    }
    method BIND(\$capture,$scope) {
        my $i = 0;
        my sub BIND($pos) {
            say "BINDING";
            $pos.BIND($scope,$capture.positional($i.FETCH));
            $i = &infix:<+>:(int,int)($i.FETCH,1);
        }
        map(&BIND,self.positionals);
    }
}
role Param {
    has $.name;
}
role Ref {
    Ref.^compose_role(::Param);
    method BIND($scope,$arg) {
        say "in Ref.BIND";
        $scope.{self.name.FETCH} := $arg;
    }
}
role ReadonlyWrapper {
    has $.value;
    method STORE() {
    }
    method FETCH() {
        return (|$.value)
    }
}
role Readonly {
}
my $sig = Signature.new();
my $code = sub () {
    say "foo: $foo";
}
$sig.positionals = ::Array.new;
my $param1 = Ref.new;
$param1.name = '$foo';
$sig.positionals.push($param1.FETCH);
my $code2 = ::Code.new(:outer($code.outer),:mold($code.mold),:signature((|$sig)));
$code2.(1);
