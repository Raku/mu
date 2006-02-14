#~/usr/bin/perl

package P6RulesInP5::Match;
use base qw/Class::Accessor/;
use overload
    'bool'  => 'as_bool',
    '""'    => 'as_string',
    '@{}'   => 'as_array',
    '%{}'   => 'as_hash'
  ; 
use strict;
use warnings;
BEGIN {
  __PACKAGE__->mk_accessors(qw/
    target
    from
    position
    coroutine
    capture
    hash
    integer
    number
    /
}

sub new {
  my $class = shift;
  my $object = bless {}, $class;
  $object;
}

sub newfrom {
  my ( $matchObj, $fromd, $grammar ) = @_;
  my $from;

  if ( $matchObj not isa "P6RulesInP5::Match" )
  {
    $target = $matchObj;
    $from = -1;
    if ( not $grammar)
    {
      $grammar = "P6RulesInP5::Rule";
    }
  }
  else
  {
    if ( not $grammar)
    {
      $grammar = classname of mod;
    }
    $target = $matchObj->target();
    $from = $matchObj->position();
  }

  $me = new $grammar;
  $me->target($target);
  $me->from($from);
  my $position = -1;
  $me->position($position);

  $from = $fromd if ( $fromd and $from < 0)

  return ($me, $target, $from, $position)
}

#Tell a Match object to continue the previous match from where it left off.
sub next {
  my ( $self ) =  @_;

  my $coroutine = $self->coroutine();
  if ( $self->coroutine() )
  {
    #execute coroutine
    #&$coroutine();
  }
  else
  {
    $self->position(-1);
    return ();
  }
}

sub to {
  my ( $self ) =  @_;
  return $self->position();
}

sub as_bool {
  my ( $self ) =  @_;
  return $self->position() > 0;
}

sub as_integer {
  my ( $self ) =  @_;

}

sub as_number {
  my ( $self ) =  @_;

}

sub as_string {
  my ( $self ) =  @_;
  my $target = $self->target();
  my $from = $self->from();
  my $position = $self->position();
  return ("") if $position < 0 or $position <= $from;
  return substr $target, $from, $position;  
}

#get_string_keyed_int
#get_pmc_keyed_int
#set_pmc_keyed_int
#delete_keyed_int
#defined_keyed_int

sub get_hash {
  my ( $self ) =  @_;
  return $self->hash();
}

sub get_array {
  my ( $self ) =  @_;
  return $self->capture();
}
