#  -*- perl -*-

use strict;
use Test::More tests => 17;

use_ok("Kwid::DOM::Node");
eval { Kwid::DOM::Node->new(); };
like($@, qr/attempt/, "Kwid::DOM::Node->new() - dies correctly");

use_ok("Kwid::DOM::Element");
$Tree::DAG_Node::DEBUG = 1;
my $node = Kwid::DOM::Element->new
    ({ name => "sect1",
       source => "=head1 ",  # text "eaten" by this node
     });
isa_ok($node, "Kwid::DOM::Node", "->new()");
is($node->source, "=head1 ", "->source() (::Element)");

my $title = Kwid::DOM::Element->new ({ name => "title" });
$node->add_daughter($title);

use_ok("Kwid::DOM::Text");
my $text = Kwid::DOM::Text->new ("NAME");
is($text->source, "NAME", "->source() (::Text)");
is($text->content, "NAME", "->content() (::Text)");

$title->add_daughter($text);

use_ok("Kwid::DOM");

my $kwom = Kwid::DOM->new();
isa_ok($kwom, "Kwid::DOM", "new DOM");

$kwom->root($node);



my $para = Kwid::DOM::Element->new({ name => "para",
				     source => "\n\n" });
$node->add_daughter($para);
my ($foo, $pi);
$para->add_daughter($foo = Kwid::DOM::Text->new
		    ({ content => "foo" }));

$node->add_daughter
    ($pi = Kwid::DOM::PI->new({ source => "\n\n=cut"}));

my @nodes;
$node->walk_down({ 'callback' => sub {
		       push @nodes, $_[0];
		   } });

is_deeply(\@nodes, [$node, $title, $text, $para, $foo, $pi],
	  "walk_down");
