use v6-alpha;

use Test;

=kwid

Parse problem when pod inside a multi-line hash-def expression.

=cut

plan 3;

my $mysub = {

    1;

=pod

=cut

};

ok "anon sub def parses when pod block is within it";

my $myhash = {

    'baz' => 3,

=pod

=cut

};

ok "anon hash def parses when pod block is within it";

my $myary = [

    4,

=pod

=cut

];

ok "anon array def parses when pod block is within it";
