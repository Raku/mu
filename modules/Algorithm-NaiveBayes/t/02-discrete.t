#!/usr/bin/perl6

use Test;
BEGIN { plan 4 }
use Algorithm::NaiveBayes;

ok 1; # If we made it this far, we're loaded.

my Algorithm::NaiveBayes $nb .= new(:model_type(Discrete));
ok $nb;

# This example data comes from the tests in Vlado Keselj's AI::NaiveBayes1 module
$nb.add_instance(attributes=>{model=>'H',place=>'B'},label=>'repairs=Y') for 1..30;
$nb.add_instance(attributes=>{model=>'H',place=>'B'},label=>'repairs=N') for 1..10;
$nb.add_instance(attributes=>{model=>'H',place=>'N'},label=>'repairs=Y') for 1..18;
$nb.add_instance(attributes=>{model=>'H',place=>'N'},label=>'repairs=N') for 1..16;
$nb.add_instance(attributes=>{model=>'T',place=>'B'},label=>'repairs=Y') for 1..22;
$nb.add_instance(attributes=>{model=>'T',place=>'B'},label=>'repairs=N') for 1..14;
$nb.add_instance(attributes=>{model=>'T',place=>'N'},label=>'repairs=Y') for 1.. 6;
$nb.add_instance(attributes=>{model=>'T',place=>'N'},label=>'repairs=N') for 1..84;

is +$nb.labels, 2;

$nb.train();

my $result = $nb.predict(attributes => {model=>'T', place => 'B'});
ok $result.{'repairs=Y'} > $result.{'repairs=N'};
