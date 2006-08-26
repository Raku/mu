use v6-alpha;
use Test;

plan 3;

=pod

Tests for CGI.new

=cut

use CGI; pass "(dummy instead of broken use_ok)";

my $q = CGI.new( rm => 'cgiapp' );
is($q.param('rm'),'cgiapp', "initializing new with a hash works");

my $q2 = CGI.new;
is($q2.param('rm'),undef, "a second object starts out undef");
