#!/usr/bin/pugs

use v6;
use Test;

plan 3;

=pod

Very basic tests for WTemplate

=cut

#use_ok('WTemplate');
skip_rest "unreliable test results"; exit;

if(eval('!("a" ~~ /a/)')) { skip_rest "skipped tests - rules support appears to be missing"; exit }

my $template;
my %var;

$template='<server:text id="var" />';
%var{'var'} = 'value';

is($template.fill_with(%var), 'value', 'element replaced correctly');

$template='<server:repeater id="list"><server:text id="v" /></server:repeater>';
%var{'list'} = [{v => '1'}, {v => '2'}];

is($template.fill_with(%var), '12', 'block replaced correctly');
