#!/usr/bin/pugs

use v6;
use CGI-0.0.1; set_url_encoding('utf-8');
use WTemplate;

my %variables;
%variables{'title'} = "WTemplate example";
%variables{'familyname'} = "Barthazi";
%variables{'firstname'} = "Andras";
%variables{'todo'} = [ { item => 'clean up the code' },
                       { item => 'extend the functionality of the widgets' }
                     ];

header(charset=>'utf-8').print;

(slurp 'example.tpl').fill_with(%variables).say;
