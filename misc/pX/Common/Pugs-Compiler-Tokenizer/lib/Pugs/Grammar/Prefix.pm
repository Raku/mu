package Pugs::Grammar::Prefix;
use strict;
use warnings;
use Pugs::Compiler::Rule;
use base qw(Pugs::Grammar::Base);

# TODO - generate AST
# TODO - prefix:{'+'}
# TODO - ~ ? 

our %hash = (
    'prefix:<+>' => Pugs::Compiler::Rule->compile( '
                { return { op => "prefix:<+>" ,} }
            ', 
            grammar => 'Pugs::Grammar::Prefix',
        ),
    '+' => Pugs::Compiler::Rule->compile( '
                { return { op => "prefix:<+>" ,} }
            ', 
            grammar => 'Pugs::Grammar::Prefix',
        ),
    'prefix:<->' => Pugs::Compiler::Rule->compile( '
                { return { op => "prefix:<->" ,} }
            ', 
            grammar => 'Pugs::Grammar::Prefix',
        ),
    '-' => Pugs::Compiler::Rule->compile( '
                { return { op => "prefix:<->" ,} }
            ', 
            grammar => 'Pugs::Grammar::Prefix',
        ),
);

sub capture {
    # print Dumper ${$_[0]}->{match}[0]{match}[1]{capture}; 
    return ${$_[0]}->{match}[0]{match}[1]{capture};
}

*parse = Pugs::Compiler::Rule->compile( '
    %Pugs::Grammar::Prefix::hash
    { return Pugs::Grammar::Prefix::capture( $/ ) }
' )->code;

1;
