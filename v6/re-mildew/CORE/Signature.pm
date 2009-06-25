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
    has $.params is rw;
    method ACCEPTS(\$capture) {
        my $i = 0;
        my $named = 0;
        my $ok = ::True;
        {
            map(sub ($param) {
                if $param.ACCEPTS($capture,$i,$named) {
                } else {
                    ::Exception.new.throw;
                }
            },self.params);
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
        map(sub ($param) {
            $param.BIND($scope,$capture,$i);
        },self.params);
    }
    method BUILDALL() {
        self.params = ::Array.new;
    }
}


role Param {
    has $.variable;
    has $.default_value;
    has $.type;
    method BUILDALL() {
        $.type = ::Any.new;
    }
    method register($sig) {
        $sig.params.push((|self));
    }
}
role Positional {
    Positional.^compose_role(::Param);
    has $.name;
    method BIND($scope,$capture,$i is ref) {
        if $capture.named($.name.FETCH) {
            if self.variable {
                $scope.{self.variable.FETCH} := self.wrap($capture.named($.name.FETCH));
            }
        } elsif &infix:<<<>>:(int,int)($i,$capture.elems) {
            if self.variable {
                $scope.{self.variable.FETCH} := self.wrap($capture.positional($i.FETCH));
            }
            $i = &infix:<+>:(int,int)($i.FETCH,1);
        } elsif $.default_value {
            my $default_value = self.default_value;
            if self.variable {
                $scope.{self.variable.FETCH} := self.wrap($default_value.());
            }
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
    method BIND($scope,$capture,$i) {
        my $arg = $capture.named(self.name.FETCH);
        my $wrapper = ReadonlyWrapper.new;
        $wrapper.value = $arg;
        $wrapper.^!is_container = 1;
        $wrapper.FETCH;
        $scope.{self.variable.FETCH} := (|$wrapper);
    }
    method ACCEPTS($capture,$i is ref,$named is ref) {
        if $capture.named($.name.FETCH) {
            $named = &infix:<+>:(int,int)($named.FETCH,1);
        }
        ::True;
    }
}
role WholeCaptureParam {
    has $.name;
    WholeCaptureParam.^compose_role(::Param);
    method ACCEPTS($capture,$i is ref,$named is ref) {
        $i = $capture.elems;
        $named = $capture.named_count;
    }
    method BIND($scope,$capture,$i) {
        $scope.{self.variable.FETCH} = $capture;
    }
}

$LexicalPrelude.{'WholeCaptureParam'} := ::WholeCaptureParam;
$LexicalPrelude.{'NamedReadonlyParam'} := ::NamedReadonlyParam;
$LexicalPrelude.{'ReadonlyParam'} := ::ReadonlyParam;
$LexicalPrelude.{'RefParam'} := ::RefParam;
$LexicalPrelude.{'Signature'} := ::Signature;

