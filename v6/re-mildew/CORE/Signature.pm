role Signature {
    has @.positionals;
    method ACCEPTS(\$capture) {
        @.positionals.elems == $capture.elems;
    }
    method BIND(\$capture,$scope) {
        my $i = 0;
        my sub BIND($pos) {
            $pos.BIND($scope,$capture.positional($i.FETCH));
            $i = &infix:<+>:(int,int)($i.FETCH,1);
        }
        map(&BIND,self.positionals);
    }
}
role Param {
    has $.name;
}
role RefParam {
    RefParam.^compose_role(::Param);
    method BIND($scope,$arg) {
        $scope.{self.name.FETCH} := $arg;
    }
}
role ReadonlyWrapper {
    has $.value;
    method FETCH() {
        $!value;
    }
    method STORE($value) {
        ::Exception.new.throw;
    }
}
role ReadonlyParam {
    ReadonlyParam.^compose_role(::Param);
    method BIND($scope,$arg) {
        my $wrapper = ReadonlyWrapper.new;
        $wrapper.value = (|$arg);
        $wrapper.^!is_container = 1;
        $wrapper.FETCH;
        $scope.{self.name.FETCH} := (|$wrapper);
    }
}

$LexicalPrelude.{'ReadonlyParam'} := ::ReadonlyParam;
$LexicalPrelude.{'RefParam'} := ::RefParam;
$LexicalPrelude.{'Signature'} := ::Signature;

