use t::lib::Regex tests => 10;

run_tests;

__DATA__

=== TEST 1:
--- rule: 'ab cd'
--- match: 'ab cd' 'ab   cd'



=== TEST 2:
--- rule: "ab '='"
--- match: 'ab=' 'ab =' 'ab  ='



=== TEST 3:
--- rule: "ab ';'"
--- match: 'ab;' 'ab ;'



=== TEST 4:
--- rule: "<ident> ';'"
--- match: "abc;" "abc ;"



=== TEST 5:
--- rule: "grammar <ident> ';'"
--- match: 'grammar Perl6;'

