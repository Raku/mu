#  -*- perl -*-

use Test::More tests => 6;
use YAML;
BEGIN{
    use_ok("Perldoc::DOM");
    use_ok("Perldoc::Receiver");
}

use Storable qw(retrieve);

SKIP:{
    my $kwom;
    eval { $kwom = retrieve "t/kwom.pm3" };
    skip "First test didn't work (?)", 3 if $@;

    # 1. test that the receiver utility functions don't exist because
    # the magic happens in the Sender class ;)

    eval("package Foo; use base qw(Perldoc::Receiver); "
	 ."sub new { bless {}, shift };");

    my $foo = new Foo;
    eval { $kwom->receiver($foo); };
    is($@,"","noop Perldoc::Receiver junks events");
    is_deeply($foo,Foo->new(),"no handlers => no effect");

    # 2. test that the DOM can turn received events into a DOM tree

    my $kwom2 = Perldoc::DOM->new();

    #local($Perldoc::Sender::DEBUG) = 1;
    $kwom->receiver($kwom2);
    eval { $kwom->send_all; };
    is($@, "", "dumped self without getting an exception");
    $kwom->reset;
    is(Dump($kwom2), Dump($kwom), "Simple DOM tree can round trip")
	or do {
    my $d1 = Dump($kwom);
    my $d2 = Dump($kwom2);
    open D1, ">/tmp/d1";
    open D2, ">/tmp/d2";
	print D1 $d1;
	print D2 $d2;
	system("diff -u /tmp/d1 /tmp/d2");
    };
}
