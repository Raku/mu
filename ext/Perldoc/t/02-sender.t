#  -*- perl -*-

use Test::More tests => 32;

BEGIN { use_ok("Perldoc::Sender") };
use Storable;
use YAML;

{
    package MySender;
    use Spiffy qw(field);
    use base qw(Spiffy);
    use base qw(Perldoc::Sender);

    field "colour";
    sub do_it {
	my $self = shift;
	$self->send(@_);
    }
}

{
    package MyReceiver;
    use Spiffy qw(field);
    use base qw(Spiffy);
    field 'events';
    our $AUTOLOAD;
    sub new {
	bless {events=>[]},__PACKAGE__;
    }
    sub AUTOLOAD {
	my $self = shift;
	$AUTOLOAD =~ s{.*::}{};
	push @{$self->events}, [$AUTOLOAD => @_];
    }
    sub can { 1 }
}

my $sender = MySender->new(colour => "orange");
isa_ok($sender, "Spiffy", "MySender");
isa_ok($sender, "Perldoc::Sender", "MySender");
is($sender->colour, "orange", "sanity");

my $receiver = MyReceiver->new();
isa_ok($receiver, "Spiffy", "MyReceiver");
is_deeply($receiver->events, [], "sanity");

# test the output api...
eval { $sender->do_it("start_document") };
like($@, qr/no receiver/, "need a receiver");

$sender->receiver($receiver);

$sender->do_it("start_element", "perldoc");
$sender->do_it("start_element", "sect1");
$sender->do_it("start_element", "title");
$sender->do_it("characters", "bert");
$sender->do_it("end_element", "title");
$sender->do_it("start_element", "para");
$sender->do_it("characters", "I like Bert, he's my friend.");
$sender->do_it("end_document");

is_deeply($receiver->events, Load(<<YAML), "utility class seems to work as advertised");
- [ start_document ]
- [ start_element, perldoc ]
- [ start_element, sect1 ]
- [ start_element, title ]
- [ characters, bert ]
- [ end_element, title ]
- [ start_element, para ]
- [ characters, "I like Bert, he's my friend." ]
- [ end_element, para ]
- [ end_element, sect1 ]
- [ end_element, perldoc ]
- [ end_document ]
YAML

