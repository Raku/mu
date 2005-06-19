#  -*- perl -*-

use v6;
use Test;

plan 12;
use_ok("Perldoc::DOM::Node");

dies_ok { Perldoc::DOM::Node.new(); },
    "Perldoc::DOM::Node.new() - dies";

use_ok("Perldoc::DOM::Element");

my $node = Perldoc::DOM::Element.new ( :name<sect1>
				       :source("=head1 ") );

isa_ok($node, "Perldoc::DOM::Node", "Element.new()");

is($node.source, "=head1 ", "->source() (::Element)");
is($node.name, "sect1", "->name() (::Element)");

my $title = Perldoc::DOM::Element.new( :name<title> );
$node.add_daughter($title);

use_ok("Perldoc::DOM::Text");

my $text = Perldoc::DOM::Text.new(:content("NAME"));

is($text.source, "NAME", "->source() (::Text)");
is($text.content, "NAME", "->content() (::Text)");

$title.add_daughter($text);

use_ok("Perldoc::DOM");

my $kwom = Perldoc::DOM.new();
isa_ok($kwom, "Perldoc::DOM", "new DOM");

$kwom.root($node);

my $gap = Perldoc::DOM::WS.new( :content("\n\n") );

$node.add_daughter($gap);

my $para = Perldoc::DOM::Element.new( :name<para> );

$node.add_daughter($para);
my ($foo, $pi);
$para.add_daughter($foo = Perldoc::DOM::Text.new(:content("foo")));

$node.add_daughter($pi = Perldoc::DOM::PI.new(:source("\n\n=cut")));

my @nodes;
$node.walk_down(sub { push @nodes, $^node });

is_deeply(\@nodes, [$node, $title, $text, $gap, $para, $foo, $pi],
	  "walk_down");

#is_deeply($kwom, eval($kwom.perl), "Perldoc::DOM trees .perl'able");
#nstore $kwom, 't/kwom.pm3';

