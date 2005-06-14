use v6;
# based loosely on http://www.perlmonks.org/?node_id=199499 
# domm@zsi.at

my $a=<j 1 t 3 l c> >>~<< <u a h P 5 k>
>>~<< <s n e e h e> >>~<< <t o r r a r>
;$a~~s:g/\s//;$a~~s:g/\d/ /;$a.say;

