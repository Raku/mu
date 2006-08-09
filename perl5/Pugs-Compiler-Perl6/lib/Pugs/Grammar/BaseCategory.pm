package Pugs::Grammar::BaseCategory;
use strict;
use warnings;
use Pugs::Compiler::Regex;
use Pugs::Compiler::Token;
use base qw(Pugs::Grammar::Base);
use Data::Dumper;

# TODO - redefine <ws> to test Pod.pm after each \n

Pugs::Compiler::Regex->install( 
    comment => q^
        .*?
        \\n   
    ^ );

Pugs::Compiler::Regex->install( 
    pod_begin => q^
        .*?
        \\n =end  
    ^ );

Pugs::Compiler::Regex->install( 
    pod_other => q^
        .*?
        \\n =cut  
    ^ );

Pugs::Compiler::Token->install( 
    ws1 => q^
    |   \\# <comment>
    |   \\n [
            | =begin  <pod_begin>
            | =\\w    <pod_other>
            | <''>
            ]
    |   \\s
    ^ );

Pugs::Compiler::Regex->install( 
    ws => ' <ws1>+ ' 
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
