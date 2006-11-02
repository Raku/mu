package Pugs::Grammar::BaseCategory;
use strict;
use warnings;
use Pugs::Compiler::Regex;
use Pugs::Compiler::Token;
use base qw(Pugs::Grammar::Base);
use Data::Dumper;

Pugs::Compiler::Token->install( 
    ws => ' <?Pugs::Grammar::Rule.ws> ' 
);

sub add_rule {
    my ( $class, $key, $rule ) = @_;
    no strict qw( refs );
    #print "add_rule [". $class . "::hash{$key} /$rule/\n";
    ${ $class . "::hash" }{$key} = $rule;
}

sub recompile {
    my ( $class ) = @_;
    no strict qw( refs );
    #print "creating ${class}::parse()\n";
    *{"${class}::parse"} = Pugs::Compiler::Token->compile( '
        <%' . $class . '::hash>
        { 
            #print "BaseCategory matched hash ", Dumper( $_[0]->data );
            return $/->{\'' . $class . '::hash\'}->();
        }
    ' )->code;
}

1;
