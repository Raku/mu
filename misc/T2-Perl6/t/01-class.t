#

use Test::More tests => 2;

use lib "../Perl6-MetaModel/lib";

use T2::Schema;
use T2::Perl6;

my $schema = T2::Schema->new( site_name => "test",
                );

$schema->add_class_from_schema( T2::Perl6::Generated => $T2::Perl6::schema );

my $t2p6 = T2::Perl6->new( schema => $schema );

$t2p6->generate_p6mm();

my $t2p6g = T2::Perl6::Generated->new();
isa_ok($t2p6g, "T2::Perl6::Generated", "it worked!  :-D");

can_ok($t2p6g, "generate_p6mm");

