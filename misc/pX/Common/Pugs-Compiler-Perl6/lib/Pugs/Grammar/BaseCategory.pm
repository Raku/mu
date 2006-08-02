package Pugs::Grammar::BaseCategory;
use strict;
use warnings;
#use Pugs::Compiler::Rule;
use Pugs::Compiler::Regex;
use base qw(Pugs::Grammar::Base);
use Data::Dumper;

*ws = Pugs::Compiler::Regex->compile( '
            (?:
                # pod start
                (?: \n | ^ ) \= \w  
                # pod body
                .*?
                # pod end
                \n \= (?: end | cut ) (?-s:.)*   
                
            |
                # normal space
                \s            
            |
                # a comment until end-of-line
                \# (?-s:.)*   
            )+
', 
    { Perl5 => 1 } 
)->code;
    
sub add_rule {
    my ( $class, $key, $rule ) = @_;
    no strict qw( refs );
    ${"${class}::hash"}{$key} = Pugs::Compiler::Regex->compile( 
        $rule, 
        { grammar => $class },
    );
}

#sub capture {
#    print "capture1: ", Dumper( $_[0]->data ); 
#    return ${$_[0]};
#}

sub recompile {
    my ( $class ) = @_;
    no strict qw( refs );
    #print "creating ${class}::parse()\n";
    *{"${class}::parse"} = Pugs::Compiler::Regex->compile( '
        %' . $class . '::hash
        { 
            #print "matching hash ", Dumper( $_[0]->data );
            return $();
        }
    ' )->code;
}

1;
