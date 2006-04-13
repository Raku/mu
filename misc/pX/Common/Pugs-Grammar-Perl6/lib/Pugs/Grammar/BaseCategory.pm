package Pugs::Grammar::BaseCategory;
use strict;
use warnings;
use Pugs::Compiler::Rule;
use base qw(Pugs::Grammar::Base);

sub add_rule {
    my ( $class, $key, $rule ) = @_;
    no strict qw( refs );
    ${"${class}::hash"}{$key} = Pugs::Compiler::Rule->compile( 
        $rule, 
        grammar => $class,
    );
}

sub capture {
    # print Dumper ${$_[0]}->{match}[0]{match}[1]{capture}; 
    return ${$_[0]}->{match}[0]{match}[1]{capture};
}

sub recompile {
    my ( $class ) = @_;
    no strict qw( refs );
    #print "creating ${class}::parse()\n";
    *{"${class}::parse"} = Pugs::Compiler::Rule->compile( '
        %' . $class . '::hash
        { return Pugs::Grammar::BaseCategory::capture( $/ ) }
    ' )->code;
}

1;
