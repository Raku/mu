package Perldoc::DOM;
use Spiffy -Base;

use base 'Perldoc::Sender';
use base 'Perldoc::Receiver';

use Perldoc::DOM::Node;
use Perldoc::DOM::Element;
use Perldoc::DOM::PI;
use Perldoc::DOM::Text;

=head1 NAME

Perldoc::DOM - Represent a Perldoc document, DOM-style

=head1 SYNOPSIS

 $kwoc = new Perldoc::DOM();

 my $body = $kwoc->root();
 my @next = $body->daughters();

 my $node = $kwoc->klink("S09#//para/");  # KLINK lookup

=head1 DESCRIPTION

A Perldoc::DOM is a directed acyclic graph, which is a Computer
Scientist's way of saying "tree" (cue: the Fast Show "aliens that say
'tree' skit").

=head1 CREATING A Perldoc::DOM TREE

C<Perldoc::DOM> trees are seldom created using the C<Tree::DAG_Node>
interface.

Normally, they will be constructed as a series of events fired in by a
L<Perldoc::Sender>, such as another L<Perldoc::DOM>, a
L<Perldoc::Preprocessor>, or a L<Perldoc::Parser>.

=cut

field 'root';  # is "Perldoc::DOM::Element"

sub new {
    my $class = ref $self || $self;

    $self = super;

    $self->root(Perldoc::DOM::Element->new({name => "pod"}));

    return $self;
}

field 'dom_sendstate';

use Scalar::Util qw(blessed);

sub send_one {
    my $dss = $self->dom_sendstate;
    if ( !$dss ) {
	$self->dom_sendstate
	    ($dss =
	     { head => undef,
	       state => undef,
	     });
    }
    local($YAML::UseHeader) = 1;
    #kill 2, $$ if $dss->{state} eq "post";
    #print STDERR "state: { state => $dss->{state}, head => ".(ref($dss->{head})||$dss->{head}||"undef")." }\n";

    if ( !$dss->{state} ) {
	$dss->{state} = "pre";
	$self->send("start_document");
	$dss->{head} = $self->root;
    } elsif ( $dss->{state} eq "pre" and $dss->{head} ) {

	if ( $dss->{head}->isa("Perldoc::DOM::Element") ) {
	    $self->send("start_element",
			$dss->{head}->name,
			$dss->{head}->dom_attr);
	    $dss->{state} = "pre";
	    $dss->{head} = (($dss->{head}->daughters)[0]) ||
		(($dss->{state} = "post"), $dss->{head});
	} else {
	    $self->send($dss->{head}->event_type,
			$dss->{head}->dom_attr);
	    $dss->{head} = $dss->{head}->right_sister ||
		(($dss->{state} = "post"), $dss->{head}->mother);
	}

    } elsif ( $dss->{state} eq "post" ) {
	if ( $dss->{head} && $dss->{head}->name ) {
	    $self->send("end_element", $dss->{head}->name);
	    $dss->{state} = "pre";
	    $dss->{head} = $dss->{head}->right_sister ||
		(($dss->{state} = "post"), $dss->{head}->mother);
	} else {
	    $self->send("end_document");
	    delete $self->{sendstack};
	    delete $self->{dom_sendstate};
	    return 0;
	}
    }
    return 1;
}

field "dom_buildstate";

sub reset {
    delete $self->{sendstack};
    delete $self->{sendstate};
    delete $self->{dom_sendstate};
    delete $self->{receiver};
}

sub start_document {
    $self->root(undef);
    $self->dom_buildstate({ head => undef,
			  });
}

sub end_document {
    delete $self->{dom_buildstate};
}

sub make_element {
    my $name = shift;
    my $o = shift || {};
    $o->{name} = $name;
    return Perldoc::DOM::Element->new($o);
}
sub make_text {
    return Perldoc::DOM::Text->new(@_);
}
sub make_pi {
    return Perldoc::DOM::PI->new(@_);
}
sub make_ws {
    return Perldoc::DOM::WS->new(@_);
}

sub start_element {
    my $dbs = $self->dom_buildstate or die;
    my $node = $self->make_element(@_);

    if ( my $head = $dbs->{head} ) {
	$head->add_daughter($dbs->{head} = $node);
    } else {
	$self->root($dbs->{head} = $node);
    }
}

sub end_element {
    my $dbs = $self->dom_buildstate or die;
    $dbs->{head} or die "too many end element events!";

    $dbs->{head} = $dbs->{head}->mother
}

sub characters {
    my $dbs = $self->dom_buildstate or die;
    my $node = $self->make_text(@_);
    $dbs->{head}->add_daughter($node);
}

sub processing_instruction {
    my $dbs = $self->dom_buildstate or die;
    my $node = $self->make_pi(@_);
    $dbs->{head}->add_daughter($node) if $node;
}

sub ignorable_whitespace {
    my $dbs = $self->dom_buildstate or die;
    my $node = $self->make_ws(@_);
    $dbs->{head}->add_daughter($node) if $node;
}
