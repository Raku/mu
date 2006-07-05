  use v6-**;
  class Point3D;
  is Point;
  has $.z;
  
  method clear () {
      call; 
      $.z = 0;
  };
  
=begin
  use v5;
  package Point3D;
  use Moose;
  
  extends 'Point';
  
  has z => (isa => 'Int');
  
  override clear => sub {
      my $self = shift;
      super;
      $self->{z} = 0;
  };
=end
