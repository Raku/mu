#########################

use Test::More tests => 3;

BEGIN { use_ok('Syntax::Highlight::Perl6'); }

ok('my $foo = Syntax::Highlight::Perl6->new', 'Constructor works');
diag($foo);
ok('my $foo = Syntax::Highlight::Perl6->new(p6code => "my $foo;"; print $foo->snippet_html');
