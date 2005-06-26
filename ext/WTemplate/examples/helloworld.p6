#!/usr/bin/pugs

use v6;
use WTemplate;

my %variables;
%variables{'message'} = "Hello World!";
%variables{'todo'} = [{item => 'Call mum'}, {item => 'Dont forget my wedding'}];

(slurp 'helloworld.tpl').fill_with(%variables).say;
