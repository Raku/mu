#!/usr/bin/perl

use strict;
use warnings;

use Test::More tests => 4;
use Test::Exception;

use Perl6::MetaModel;
use Perl6::Object;

=pod

From L<A12/Default Values>

    class Hitchhiker {
        my $defaultanswer = 0;
        has $.ans1 = $defaultanswer;
        has $.ans2 = { $defaultanswer };
        has $.ans3 = { { $defaultanswer } };
        $defaultanswer = 42;
        ...
    }
    
NOTE: The metamodel does not support paramaterized 
build traits though.
    
=cut

my $default_answer = 0;
class Hitchhiker => {
    is => [ 'Perl6::Object' ],
    instance => {
        attrs => [
            [ '$.ans1' => { build => $default_answer } ],
            [ '$.ans2' => { build => sub { $default_answer } } ],         
            [ '$.ans3' => { build => sub { sub { $default_answer }} } ]                        
        ]
    }
};
$default_answer = 42;

my $hh = Hitchhiker->new();
is($hh->ans1, 0, '... the first answer is 0');
is($hh->ans2, 42, '... the second answer is 42');
is($hh->ans3->(), 42, '... the third answer is a closure around 42');

$default_answer = 45;
is($hh->ans3->(), 45, '... the third answer is still a closure around 42 (now 45)');

