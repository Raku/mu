module Perl::Compiler::CodeGen;

class NameGen {
    has $.template;
    has $.counter;
    has $.parent;
    has $.parent_ident;
    has %.names;

    submethod BUILD ($.template, ?$.counter = [0], ?$.parent, ?$.parent_ident) { }
    
    method fork($ident) {
        NameGen.new(
            template => $.template,
            counter  => $.counter,
            parent   => $?SELF,
            parent_ident => $ident,
        );
    }

    method inject($name, $value) {
        %.names{$name} = $value;
    }

    method ret($value) {
        $.parent // die "No parent at this level";
        $.parent.inject($.parent_ident, $value);
    }

    method r($name) {
        %.names{$name} //= ./newreg;
    }

    method newreg() {
        $.template($.counter[0]++);
    }
}

# vim: ft=perl6 :
