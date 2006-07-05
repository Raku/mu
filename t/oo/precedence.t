use v6-pugs;

use Test;

# L<"http://use.perl.org/~autrijus/journal/25365">
# Closure composers like anonymous sub, class and module always trumps hash
# dereferences:
# 
#   sub{...}
#   module{...}
#   class{...}

plan 2;

ok(sub { 42 }(), 'sub {...} works');
ok(sub{ 42 }(),  'sub{...} works');
