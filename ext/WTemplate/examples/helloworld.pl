use v6-alpha;
use WTemplate;

my %variables;
%variables{'message'} = "Hello World!";
%variables{'todo'} = [{item => 'Call mum'}, {item => 'Dont forget my wedding'}];

(slurp 'helloworld.tpl').fill_with(%variables).say;
