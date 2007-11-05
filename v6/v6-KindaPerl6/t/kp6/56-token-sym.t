say "1..1";

say "not ok 1 # add_token_variant, needs to be implemented in Class MultiToken, Runtime::Perl6::Multi";

# class Main {
#
# say "1..1";
#
# say "not ok 1 # add_token_variant, needs to be implemented in Class MultiToken, Runtime::Perl6::Multi
#
# method ok { say "ok 1" };
#
# proto token xyz {};
#
# token xyz:sym<abc>   { abc }
# token xyz:abf        { <Main.ok> abf }
# token xyz:sym<abger> { abger }
# token xyz:sym« << »  { << }
#
# $_ = "abfbbb";
# # Main.xyz;   XXX - AST bug
# xyz( $_, 0 );  # XXX - what is the calling convention for regexes?
#
# }
#
