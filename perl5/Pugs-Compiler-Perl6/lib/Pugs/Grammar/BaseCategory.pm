package Pugs::Grammar::BaseCategory;
use strict;
use warnings;
use Pugs::Compiler::Regex;
use Pugs::Compiler::Token;
use base qw(Pugs::Grammar::Base);
use Data::Dumper;

# TODO - redefine <ws> to test Pod.pm after each \n

Pugs::Compiler::Regex->install( 
    to_end_of_line => '
        [^\n]*? 
        (?: \n | $ )
    ', { Perl5 => 1 } );

Pugs::Compiler::Regex->install( 
    to_end_of_pod => '
        .*? 
        (?= \n = ) 
    ', { Perl5 => 1 } );

Pugs::Compiler::Token->install( 
    pod_begin => q^
    |   \\n =end  
    |   . <?to_end_of_pod> <?pod_begin>
    ^ );

Pugs::Compiler::Token->install( 
    pod_other => q^
    |   \\n =cut  
    |   . <?to_end_of_pod> <?pod_other>
    ^ );

Pugs::Compiler::Token->install( 
    ws1 => q^
    |   \\# <to_end_of_line>
    |   \\n [
            | =begin  <pod_begin>
            | =kwid   <pod_other>
            | =pod    <pod_other>
            | =for    <pod_other>
            | <''>
            ]
    |   \\s
    ^ );

Pugs::Compiler::Token->install( 
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
