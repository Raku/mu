#########################

use Test::More tests => 3;

BEGIN { use_ok('Perl6::Highlight'); }

ok('my $foo = Perl6::Highlight->new', 'Constructor works');
diag($foo);
ok('my $foo = Perl6::Highlight->new(p6code => "my $foo;"; print $foo->snippet_html');
