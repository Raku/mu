#  -*- perl -*-

use strict;
use Test::More tests => 13;
use Storable qw(nstore dclone);

use_ok("Perldoc::DOM::Node");
eval { Perldoc::DOM::Node->new(); };
like($@, qr/attempt/, "Perldoc::DOM::Node->new() - dies correctly");

use_ok("Perldoc::DOM::Element");
#$Tree::DAG_Node::DEBUG = 1;
my $node = Perldoc::DOM::Element->new
    ({ name => "sect1",
       source => "=head1 ",  # text "eaten" by this node
     });
isa_ok($node, "Perldoc::DOM::Node", "->new()");
is($node->source, "=head1 ", "->source() (::Element)");
is($node->name, "sect1", "->name() (::Element)");

my $title = Perldoc::DOM::Element->new ({ name => "title" });
$node->add_daughter($title);

use_ok("Perldoc::DOM::Text");
my $text = Perldoc::DOM::Text->new ("NAME");
is($text->source, "NAME", "->source() (::Text)");
is($text->content, "NAME", "->content() (::Text)");

$title->add_daughter($text);

use_ok("Perldoc::DOM");

my $kwom = Perldoc::DOM->new();
isa_ok($kwom, "Perldoc::DOM", "new DOM");

$kwom->root($node);

my $para = Perldoc::DOM::Element->new({ name => "para",
					source => "\n\n" });
$node->add_daughter($para);
my ($foo, $pi);
$para->add_daughter($foo = Perldoc::DOM::Text->new
		    ({ content => "foo" }));

$node->add_daughter
    ($pi = Perldoc::DOM::PI->new({ source => "\n\n=cut"}));

my @nodes;
$node->walk_down({ 'callback' => sub {
		       push @nodes, $_[0];
		   } });

is_deeply(\@nodes, [$node, $title, $text, $para, $foo, $pi],
	  "walk_down");

is_deeply($kwom, dclone($kwom), "Perldoc::DOM trees storable");

nstore $kwom, 't/kwom.pm3';

