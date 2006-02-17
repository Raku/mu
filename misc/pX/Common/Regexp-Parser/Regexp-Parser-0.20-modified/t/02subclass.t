# Before `make install' is performed this script should be runnable with
# `make test'. After `make install' it should work as `perl Regexp-Parser.t'

#########################

# change 'tests => 1' to 'tests => last_test_to_print';

use Test;
BEGIN { plan tests => 12 };

{
  package Regexp::AndBranch::__object__;
  sub my_method { "blah" }

  package Regexp::AndBranch;
  use base 'Regexp::Parser';
  main::ok( 1 );

  sub init {
    my $self = shift;
    $self->SUPER::init();

    # X&Y = match Y if match X at the same place
    $self->add_handler('&' => sub {
      my ($S) = @_;
      push @{ $S->{next} }, qw< atom >;
      return $S->object(and => 1);
    });

    # X!Y = match Y unless match X at the same place
    $self->add_handler('!' => sub {
      my ($S) = @_;
      push @{ $S->{next} }, qw< atom >;
      return $S->object(and => 0);
    });
  }


  package Regexp::AndBranch::and;
  @ISA = qw( Regexp::Parser::branch );

  sub new {
    my ($class, $rx, $pos) = @_;
    my $self = bless {
      rx => $rx,
      flags => $rx->{flags}[-1],
      data => [ [] ],
      family => 'branch',
      branch => 1,
      neg => !$pos,
    }, $class;
    return $self;
  }

  sub raw {
    my $self = shift;
    $self->{neg} ? '!' : '&';
  }

  sub type {
    my $self = shift;
    $self->{neg} ? 'not' : 'and';
  }

  sub qr {
    my ($self) = @_;
    my @kids = @{ $self->{data} };
    my $consume = pop @kids;
    my $type = $self->{neg} ? 'unlessm' : 'ifmatch';
    $_ = $self->{rx}->object($type => 1, @$_) for @kids;
    return join "", map($_->qr, @kids), map($_->qr, @$consume);
  }
}

my $r1 = Regexp::AndBranch->new;
my $r2 = Regexp::AndBranch->new;

ok( $r1->regex('^(?:.*foo&\D*(\d+))') );
ok( $r1->visual, '^(?:.*foo&\D*(\d+))' );
ok( $r1->qr, ~~qr/^(?:(?=.*foo)\D*(\d+))/ );

ok( $r2->regex('^(?:.*foo!\D*(\d+))') );
ok( $r2->visual, '^(?:.*foo!\D*(\d+))' );
ok( $r2->qr, ~~qr/^(?:(?!.*foo)\D*(\d+))/ );

ok( "1 foo 2" =~ $r1->qr && $1, 1 );
ok( "1 foo 2" !~ $r2->qr );

ok( "1 bar 2" !~ $r1->qr );
ok( "1 bar 2" =~ $r2->qr && $1, 1 );

ok( $r1->object(and => 1)->my_method, "blah" );
