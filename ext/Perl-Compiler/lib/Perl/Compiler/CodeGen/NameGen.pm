module Perl::Compiler::CodeGen;

class Perl::Compiler::CodeGen::NameGen {
    has $.template;
    has $.counter;
    has $.parent;
    has $.parent_ident;
    has %.names;
    has %.state is rw;

    submethod BUILD ($.template, ?$.counter = [0], ?$.parent, ?$.parent_ident, ?$.state) { }
    
    method fork($ident) {
        my $ret = $?CLASS.new(
            template     => $.template,
            conter       => $.counter,
            parent       => $?SELF,
            parent_ident => $ident,
            state        => %.state,
        );
        $ret;
    }

    method inject($name, $value) {
        if $name eq 'RET' {
            $.parent // die "No parent at this level";
            if defined $.parent_ident {
                $.parent.inject($.parent_ident, $value);
            }
        }
        else {
            %.names{$name} = $value;
        }
    }

    method ret($value) {
        ./inject('RET', $value);
    }

    method r($name) {
        %.names{$name} //= ./newreg;
    }

    method newreg() {
        $.template($.counter[0]++);
    }
}

# vim: ft=perl6 :
