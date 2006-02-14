#~/usr/bin/perl

package P6RulesInP5::Rule;
use base qw/Class::Accessor P6RulesInP5::Match/;
use strict;
use warnings;
BEGIN {
  __PACKAGE__->mk_accessors(qw/
    /
}

sub new {
  my $class = shift;
  my $object = bless {}, $class;
  $object;
}

sub null {
  my ( $matchObj ) = @_
  my ($matchObj, $target, $from, $position) = P6RulesInP5::Match::newfrom( $matchObj );
  $matchObj->position($matchObj->from());
  return $matchObj;
}

sub fail {
  my ( $matchObj ) = @_
  return P6RulesInP5::Match::newfrom( $matchObj );
}

sub upper {
  my ( $matchObj ) = @_
  my ($matchObj, $target, $from, $position) = P6RulesInP5::Match::newfrom( $matchObj );
  if ( (substr $target, $from, 1) =~ /[:upper:]/ )
  {
    $matchObj->position( $matchObj->from() + 1);
  }
  return $matchObj;
}

sub lower {
  my ( $matchObj ) = @_
  my ($matchObj, $target, $from, $position) = P6RulesInP5::Match::newfrom( $matchObj );
  if ( (substr $target, $from, 1) =~ /[:lower:]/ )
  {
    $matchObj->position( $matchObj->from() + 1);
  }
  return $matchObj;
}

sub alpha {
  my ( $matchObj ) = @_
  my ($matchObj, $target, $from, $position) = P6RulesInP5::Match::newfrom( $matchObj );
  if ( (substr $target, $from, 1) =~ /[:alpha:]/ )
  {
    $matchObj->position( $matchObj->from() + 1);
  }
  return $matchObj;
}

sub digit {
  my ( $matchObj ) = @_
  my ($matchObj, $target, $from, $position) = P6RulesInP5::Match::newfrom( $matchObj );
  if ( (substr $target, $from, 1) =~ /[:digit:]/ )
  {
    $matchObj->position( $matchObj->from() + 1);
  }
  return $matchObj;
}

sub xdigit {
  my ( $matchObj ) = @_
  my ($matchObj, $target, $from, $position) = P6RulesInP5::Match::newfrom( $matchObj );
  if ( (substr $target, $from, 1) =~ /[:xdigit:]/ )
  {
    $matchObj->position( $matchObj->from() + 1);
  }
  return $matchObj;
}

sub space {
  my ( $matchObj ) = @_
  my ($matchObj, $target, $from, $position) = P6RulesInP5::Match::newfrom( $matchObj );
  if ( (substr $target, $from, 1) =~ /[:space:]/ )
  {
    $matchObj->position( $matchObj->from() + 1);
  }
  return $matchObj;
}

sub print {
  my ( $matchObj ) = @_
  my ($matchObj, $target, $from, $position) = P6RulesInP5::Match::newfrom( $matchObj );
  if ( (substr $target, $from, 1) =~ /[:print:]/ )
  {
    $matchObj->position( $matchObj->from() + 1);
  }
  return $matchObj;
}

sub graph {
  my ( $matchObj ) = @_
  my ($matchObj, $target, $from, $position) = P6RulesInP5::Match::newfrom( $matchObj );
  if ( (substr $target, $from, 1) =~ /[:graph:]/ )
  {
    $matchObj->position( $matchObj->from() + 1);
  }
  return $matchObj;
}

sub blank {
  my ( $matchObj ) = @_
  my ($matchObj, $target, $from, $position) = P6RulesInP5::Match::newfrom( $matchObj );
  if ( (substr $target, $from, 1) =~ /[:blank:]/ )
  {
    $matchObj->position( $matchObj->from() + 1);
  }
  return $matchObj;
}

sub cntrl {
  my ( $matchObj ) = @_
  my ($matchObj, $target, $from, $position) = P6RulesInP5::Match::newfrom( $matchObj );
  if ( (substr $target, $from, 1) =~ /[:cntrl:]/ )
  {
    $matchObj->position( $matchObj->from() + 1);
  }
  return $matchObj;
}

sub punct {
  my ( $matchObj ) = @_
  my ($matchObj, $target, $from, $position) = P6RulesInP5::Match::newfrom( $matchObj );
  if ( (substr $target, $from, 1) =~ /[:punct:]/ )
  {
    $matchObj->position( $matchObj->from() + 1);
  }
  return $matchObj;
}

sub alnum {
  my ( $matchObj ) = @_
  my ($matchObj, $target, $from, $position) = P6RulesInP5::Match::newfrom( $matchObj );
  if ( (substr $target, $from, 1) =~ /[:alnum:]/ )
  {
    $matchObj->position( $matchObj->from() + 1);
  }
  return $matchObj;
}

sub sp {
  my ( $matchObj ) = @_
  my ($matchObj, $target, $from, $position) = P6RulesInP5::Match::newfrom( $matchObj );
  if ( (substr $target, $from, 1) =~ / / )
  {
    $matchObj->position( $matchObj->from() + 1);
  }
  return $matchObj;
}

sub lt {
  my ( $matchObj ) = @_
  my ($matchObj, $target, $from, $position) = P6RulesInP5::Match::newfrom( $matchObj );
  if ( (substr $target, $from, 1) =~ /</ )
  {
    $matchObj->position( $matchObj->from() + 1);
  }
  return $matchObj;
}

sub gt {
  my ( $matchObj ) = @_
  my ($matchObj, $target, $from, $position) = P6RulesInP5::Match::newfrom( $matchObj );
  if ( (substr $target, $from, 1) =~ />/ )
  {
    $matchObj->position( $matchObj->from() + 1);
  }
  return $matchObj;
}

sub dot {
  my ( $matchObj ) = @_
  my ($matchObj, $target, $from, $position) = P6RulesInP5::Match::newfrom( $matchObj );
  if ( (substr $target, $from, 1) =~ /\./ )
  {
    $matchObj->position( $matchObj->from() + 1);
  }
  return $matchObj;
}

sub ws {
  my ( $matchObj ) = @_
}



















