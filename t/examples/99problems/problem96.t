use v6-alpha;
use Test;
plan 4;

# P96 (**) Syntax checker (alternative solution with difference lists)
# 
# In a certain programming language (Ada) identifiers are defined by the syntax
# diagram (railroad chart) opposite. Transform the syntax diagram into a system
# of syntax diagrams which do not contain loops; i.e. which are purely
# recursive. Using these modified diagrams, write a predicate identifier/1 that
# can check whether or not a given string is a legal identifier.
# 
# % identifier(Str) :- Str is a legal identifier

# having regexes, we have a much simpler way:

sub identifier(Str $x){
    return ? ( $x ~~ m/^<?letter> [ '_'? [<?letter> | <?digit> ]]*$/);
}

ok(identifier("abc_3f_3"), "Syntax Checker recognized valid string");
ok(!identifier('_abc'), "leading underscore rejected");
ok(!identifier('a__b'), "two adjacent underscores rejected");
ok(!identifier('abc_'), "trailing underscore rejected");

