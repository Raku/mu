package main;

use lib '../v6-MiniPerl6/lib5', 'lib5';
use strict;

use KindaPerl6::Perl5::Runtime;

package Main;
use KindaPerl6::Grammar;

use KindaPerl6::Traverse;
use KindaPerl6::Visitor::LexicalSub;
use KindaPerl6::Visitor::Perl;
use KindaPerl6::Visitor::EmitPerl5;
use KindaPerl6::Visitor::MetaClass;
use KindaPerl6::Visitor::CreateEnv;

use KindaPerl6::Grammar::Regex;
use KindaPerl6::Emitter::Token;

use Data::Dump::Streamer;

my $env1 = Pad->new( 
    outer    => undef, 
    lexicals => [
        Decl->new(  
            decl  => 'my',  
            var   => Var->new(  
                name   => 'x',  
                twigil => '',  
                sigil  => '$', 
            ),  
            type  => '', 
        ),  
    ] 
);

$env1->eval( '$x = 3' );
$env1->eval( ' print "x=$x\n" ' );   

my $env2 = Pad->new( 
    outer    => $env1, 
    lexicals => [
        Decl->new(  
            decl  => 'my',  
            var   => Var->new(  
                name   => 'y',  
                twigil => '',  
                sigil  => '$', 
            ),  
            type  => '', 
        ),  
    ] 
);

$env2->eval( '$y = 42' );
$env2->eval( ' print "y=$y x=$x\n" ' );   

$env2->add_lexicals( [
        Decl->new(  
            decl  => 'my',  
            var   => Var->new(  
                name   => 'z',  
                twigil => '',  
                sigil  => '$', 
            ),  
            type  => '', 
        ),  
    ] 
);

$env2->eval( '$z = 123' );
$env2->eval( ' print "x=$x y=$y z=$z\n" ' );   

print $env2->is_lexical( Var->new(  
                name   => 'x',  
                twigil => '',  
                sigil  => '$', 
            ) 
        )
        ? "x is lexical\n"
        : "error\n";

print $env2->is_lexical( Var->new(  
                name   => 'w',  
                twigil => '',  
                sigil  => '$', 
            ) 
        )
        ? "error\n"
        : "w is not lexical\n";

#print Dump( $env2 );
#print Dump( $env2->variable_names );
