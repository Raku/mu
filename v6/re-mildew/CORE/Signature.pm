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
        my $i = 0;
        my $named = 0;
        my $ok = ::True;
        {
            map(sub ($positional) {
                if $positional.ACCEPTS($capture,$i,$named) {
                } else {
                    ::Exception.new.throw;
                }
            },self.positionals);
            CATCH {
                $ok = ::False;
            }
        }.();
        if &infix:<==>:(int,int)($i,$capture.elems) {
            if &infix:<==>:(int,int)($named,$capture.named_count) {
                $ok;
            } else {
                ::False;
            }
        } else {
            ::False;
        }
    }
    method BIND(\$capture,$scope) {
        my $i = 0;
        map(sub ($positional) {
            $positional.BIND($scope,$capture,$i);
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
    method BUILDALL() {
        $.type = ::Any.new;
    }
}
role Positional {
    Positional.^compose_role(::Param);
    has $.name;
    method register($sig) {
        $sig.positionals.push((|self));
    }
    method BIND($scope,$capture,$i is ref) {
        if $capture.named($.name.FETCH) {
            $scope.{self.variable.FETCH} := self.wrap($capture.named($.name.FETCH));
        } elsif &infix:<<<>>:(int,int)($i,$capture.elems) {
            $scope.{self.variable.FETCH} := self.wrap($capture.positional($i.FETCH));
            $i = &infix:<+>:(int,int)($i.FETCH,1);
        } elsif $.default_value {
            my $default_value = self.default_value;
            $scope.{self.variable.FETCH} := self.wrap($default_value.());
        } else {
            return ::False;
        }
        ::True;
    }
    method ACCEPTS($capture,$i is ref,$named is ref) {
        if $capture.named($.name.FETCH) {
            $named = &infix:<+>:(int,int)($named.FETCH,1);
        } elsif &infix:<<<>>:(int,int)($i,$capture.elems) {
            if $.type.ACCEPTS($capture.positional($i.FETCH)) {
                $i = &infix:<+>:(int,int)($i,1);
            } else {
                return ::False;
            }
        } else {
            return ::False;
        }
        ::True;
    }
}
role RefParam {
    RefParam.^compose_role(::Positional);
    method wrap($arg) {
        $arg;
    }
}
role ReadonlyParam {
    ReadonlyParam.^compose_role(::Positional);
    method wrap($arg) {
        my $wrapper = ReadonlyWrapper.new;
        $wrapper.value = $arg;
        $wrapper.^!is_container = 1;
        $wrapper.FETCH;
        (|$wrapper);
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

