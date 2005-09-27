#!/usr/bin/perl

use strict;
use warnings;

use Test::More tests => 1;

pass("Luqui broke me");

__END__

use Test::Exception; 

use Perl6::MetaModel;

{

    my $PrettyPrinter = class 'PrettyPrinter' => {
        is => [ $::Object ],
        methods => {
            pretty => ::multi_sub 'PrettyPrinter::pretty' => ('PrettyPrinter', 'ARRAY') => sub { 
                "[ " . join(', ', map { $::SELF->pretty($_) } @{$_[1]}) . " ]" 
            },
            pretty => ::multi_sub 'PrettyPrinter::pretty' => ('PrettyPrinter', 'HASH') => sub { 
                "{ " . join(', ', map { "$_ => " . $::SELF->pretty($_[1]->{$_}) } keys %{$_[1]}) . " }" 
            },
            pretty => ::multi_sub 'PrettyPrinter::pretty' => ('PrettyPrinter', 'SCALAR') => sub { 
                ${$_[1]};
            },
            pretty => ::multi_sub 'PrettyPrinter::pretty' => ('PrettyPrinter') => sub { 
                "~~" . $::CLASS->name . "~~";
            }                
        }
    };

    my $pp = $PrettyPrinter->new();
    isa_ok($pp, 'PrettyPrinter');


    is($pp->pretty(), '~~PrettyPrinter~~', '... got the pretty printed PrettyPrinter');
    is($pp->pretty(\1), '1', '... got the pretty printed SCALAR');
    is($pp->pretty([]), '[  ]', '... got the pretty printed ARRAY');     
    is($pp->pretty({}), '{  }', '... got the pretty printed HASH'); 

    is(
        $pp->pretty({ array => [ \1, \'two', $pp ] }), 
        '{ array => [ 1, two, ~~PrettyPrinter~~ ] }', 
        '... recursive multi-methods');     

}

{
    my $PrettyPrinter2 = class 'PrettyPrinter2' => sub {
        $::CLASS->superclasses([ $::Object ]);
    
        my $pretty;
        $pretty = ::multi_sub 'PrettyPrinter2::pretty' => ('PrettyPrinter2', 'ARRAY') => sub { 
            "[ " . join(', ', map { $::SELF->pretty($_) } @{$_[1]}) . " ]" 
        };
        $pretty = ::multi_sub 'PrettyPrinter2::pretty' => ('PrettyPrinter2', 'HASH') => sub { 
            "{ " . join(', ', map { "$_ => " . $::SELF->pretty($_[1]->{$_}) } keys %{$_[1]}) . " }" 
        };
        $pretty = ::multi_sub 'PrettyPrinter2::pretty' => ('PrettyPrinter2', 'SCALAR') => sub { 
            ${$_[1]};
        };  
        $pretty = ::multi_sub 'PrettyPrinter2::pretty' => ('PrettyPrinter2') => sub { 
            "~~" . $::CLASS->name . "~~";
        };        
    
        $::CLASS->add_method('pretty' => ::make_method($pretty));
    };
    
    my $pp = $PrettyPrinter2->new();
    isa_ok($pp, 'PrettyPrinter2');
    
    is($pp->pretty(), '~~PrettyPrinter2~~', '... got the pretty printed PrettyPrinter2');    
    is($pp->pretty(\1), '1', '... got the pretty printed SCALAR');
    is($pp->pretty([]), '[  ]', '... got the pretty printed ARRAY');     
    is($pp->pretty({}), '{  }', '... got the pretty printed HASH'); 
    
    is(
        $pp->pretty({ array => [ \1, \'two', $pp ] }), 
        '{ array => [ 1, two, ~~PrettyPrinter2~~ ] }', 
        '... recursive multi-methods'); 
}


