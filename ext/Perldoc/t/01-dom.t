#  -*- perl -*-

use v6;
use Test;

plan 15;

# include from the top...
use Perldoc::DOM;

dies_ok { Perldoc::DOM::Node.new(); }, "Perldoc::DOM::Node.new() - dies",
    :todo;

# build from the bottom...
my $node;
lives_ok{ $node = Perldoc::DOM::Element.new( :name<sect1>,
                                             :source("=head1 ") ); },
    "::Element.new()";
isa_ok($node, "Perldoc::DOM::Element", "Element.new()");

is($node.source, "=head1 ", "->source() (::Element)");
is($node.name, "sect1", "->name() (::Element)");

my $title = Perldoc::DOM::Element.new( :name<title> );

lives_ok{ $node.add_child($title) }, "add child to node", :todo;

my $text = Perldoc::DOM::Text.new(:content("NAME"));

is($text.source, "NAME", "->source() (::Text)");
is($text.content, "NAME", "->content() (::Text)");

lives_ok{ $title.add_child($text) }, "add child to title", :todo;

my $kwom = Perldoc::DOM.new();
isa_ok($kwom, "Perldoc::DOM", "new DOM");

lives_ok { $kwom.root($node); }, "get root of kwom", :todo;

my $gap = Perldoc::DOM::WS.new( :content("\n\n") );

lives_ok{ $node.add_child($gap); }, "add gap child to node", :todo;

my $para = Perldoc::DOM::Element.new( :name<para> );

lives_ok{ $node.add_child($para); }, "add para child to node", :todo;
my ($foo, $pi);
lives_ok{
$para.add_child($foo = Perldoc::DOM::Text.new(:content("foo")));

$node.add_child($pi = Perldoc::DOM::PI.new(:source("\n\n=cut")));
}, "add more nodes to para", :todo;

my @nodes;
lives_ok{ $node.traverse(sub { push @nodes, $^node }) },
    "can traverse Tree", :todo;

#is(@nodes.join(","), "Perldoc::DOM::Element", "can walk Tree");

#is_deeply(\@nodes, [$node, $title, $text, $gap, $para, $foo, $pi],
          #"walk_down");

#is_deeply($kwom, eval($kwom.perl), "Perldoc::DOM trees .perl'able");
#nstore $kwom, 't/kwom.pm3';

