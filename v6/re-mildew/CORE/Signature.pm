role ReadonlyWrapper {
    has $.value;
    method FETCH() {
        (|$!value);
    }
    method STORE($value) {
        ::Exception.new.throw;
    }
}

role Signature {
    has $.positionals;
    has $.other;
    method ACCEPTS(\$capture) {
        if &infix:<==>:(int,int)($.positionals.elems,$capture.elems) {
        } else {
            return ::False;
        }
        my $i = 0;
        loop {
            if &infix:<==>:(int,int)($i,$capture.elems) {
                return ::True;
            } else {
                if $.positionals.[$i.FETCH].ACCEPTS($capture.positional($i.FETCH)) {
                } else {
                    return ::False;
                }
                $i = &infix:<+>:(int,int)($i.FETCH,1);
            }
        }

    }
    method BIND(\$capture,$scope) {
        my $i = 0;
        map(sub ($pos) {
            if &infix:<<<>>:(int,int)($i,$capture.elems) {
                $pos.BIND($scope,$capture.positional($i.FETCH));
            } else {
                $pos.BIND_with_default($scope);
            }
            $i = &infix:<+>:(int,int)($i.FETCH,1);
        },self.positionals);

        map(sub ($other) {
            $other.BIND($scope,$capture);
        },self.other);
    }
    method BUILDALL() {
        self.positionals = ::Array.new;
        self.other = ::Array.new;
    }
}


role Param {
    has $.variable;
    has $.default_value;
    has $.type;
    method BIND_with_default($scope) {
        my $default_value = self.default_value;
        self.BIND($scope,$default_value.());
    }
    method ACCEPTS($arg) {
        $.type.ACCEPTS($arg);
    }
    method BUILDALL() {
        $.type = ::Any.new;
    }
}
role Positional {
    Positional.^compose_role(::Param);
    method register($sig) {
        $sig.positionals.push((|self));
    }
}
role RefParam {
    RefParam.^compose_role(::Positional);
    method BIND($scope,$arg) {
        $scope.{self.variable.FETCH} := $arg;
    }
}
role ReadonlyParam {
    ReadonlyParam.^compose_role(::Positional);
    method BIND($scope,$arg) {
        my $wrapper = ReadonlyWrapper.new;
        $wrapper.value = $arg;
        $wrapper.^!is_container = 1;
        $wrapper.FETCH;
        $scope.{self.variable.FETCH} := (|$wrapper);
    }
}
role NamedReadonlyParam {
    NamedReadonlyParam.^compose_role(::Param);
    has $.name;
    method BIND($scope,$capture) {
        my $arg = $capture.named(self.name.FETCH);
        my $wrapper = ReadonlyWrapper.new;
        $wrapper.value = $arg;
        $wrapper.^!is_container = 1;
        $wrapper.FETCH;
        $scope.{self.variable.FETCH} := (|$wrapper);
    }
    method register($sig) {
        $sig.other.push((|self));
    }
}
role WholeCaptureParam {
    WholeCaptureParam.^compose_role(::Param);
    method BIND($scope,$capture) {
        $scope.{self.variable.FETCH} = $capture;
    }
    method register($sig) {
        $sig.other.push((|self));
    }
}

$LexicalPrelude.{'WholeCaptureParam'} := ::WholeCaptureParam;
$LexicalPrelude.{'NamedReadonlyParam'} := ::NamedReadonlyParam;
$LexicalPrelude.{'ReadonlyParam'} := ::ReadonlyParam;
$LexicalPrelude.{'RefParam'} := ::RefParam;
$LexicalPrelude.{'Signature'} := ::Signature;

