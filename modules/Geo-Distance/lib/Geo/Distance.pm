module Geo::Distance-0.08;
use v6;

use Math::Trig <great_circle_distance deg2rad rad2deg acos pi>;

class Geo::Distance {
  my $KILOMETER_RHO = 6371.64;

  my subtype Formula of Str where { $^formula eq "mt"|"cos"|"hsin"|"polar" }

  has Formula $.formula = "hsin" is rw;
  has %:units;

  submethod BUILD (Bool ?$no_units) {
    my $class = shift;
    my $self = bless {}, $class;
    my %args = @_;
    
    unless $no_units {
      $self->reg_unit( 'kilometer', KILOMETER_RHO );
      $self->reg_unit( 'meter', 1000, 'kilometer' );
      $self->reg_unit( 'centimeter', 100, 'meter' );
      $self->reg_unit( 'yard', 1.0936, 'meter' );
      $self->reg_unit( 'foot', 3, 'yard' );
      $self->reg_unit( 'inch', 12, 'foot' );
      $self->reg_unit( 'light_second', (1/298000), 'kilometer' );
      $self->reg_unit( 'mile', 0.6214, 'kilometer' );
      $self->reg_unit( 'nautical mile', 1.852, 'kilometer' );
    }

    # Number of units in a single degree (lat or lon) at the equator.
    # Derived from: $geo->distance( 'kilometer', 10,0, 11,0 ) / $geo->{units}->{kilometer}
    $self->{deg_ratio} = 0.0174532925199433;
  }


  sub reg_unit(Str $unit, Num $amount, Str ?$base) {
    if $base {
      die "The unit \"$base\" is not defined."
	unless %:units{$base};
      %:units{$unit} = %:units{$base} * $amount;
    } else{
      # Make a new unit, or update an existing one.
      %:units{$unit} = $amount;
    }
  }

  sub distance(
    Str $unit,
    Num $lon1, Num $lat1,
    Num $lon2, Num $lat2
  ) {
    die "Unknown unit tye \"$unit\""
      unless $unit = %:units{$unit};

    given $.formula {
      when "mt" {
	return great_circle_distance(
	  deg2rad $lon1,
	  deg2rad 90 - $lat1, 
	  deg2rad $lon2, 
	  deg2rad 90 - $lat2, 
	  $unit
      }

      $lon1 = deg2rad($lon1); $lat1 = deg2rad($lat1);
      $lon2 = deg2rad($lon2); $lat2 = deg2rad($lat2);

      when "cos" {
	my $a = sin $lat1 * sin $lat2;
	my $b = cos $lat1 * cos $lat2 * cos($lon2 - $lon1);
	$c = acos($a + $b);
	return $unit * $c;
      } when "hsin" {
	my $dlon = $lon2 - $lon1;
	my $dlat = $lat2 - $lat1;
	my $a = (sin($dlat/2)) ** 2 + cos($lat1) * cos($lat2) * (sin($dlon/2)) ** 2;
	$c = 2 * atan2(sqrt($a), sqrt(1-$a));
	return $unit * $c;
      } when "polar" {
	my $a = pi/2 - $lat1;
	my $b = pi/2 - $lat2;
	$c = sqrt( $a ** 2 + $b ** 2 - 2 * $a * $b * cos($lon2 - $lon1) );
	return $unit * $c;
      }
    }
  }

  method closest(
    Num $distance, Str $unit, Num $lon, Num $lat,
    Bool ?$sort = 1,
    Num ?$count,
    Hash *@locations,
  ) {
    # Calculate distances.
    my @closest;
    for @locations -> $location {
      $location<distance> = .distance(
	$unit, $lon, $lat,
	$location<lon>, $location<lat>
      );

      push @closest, $location
	if $location<distance> <= $distance;
    }

    # Sort.
    @closest .= sort { $^a<distance> <=> $^b<distance> } if $sort;
    # Splice.
    @closest = @closest[0..^$count] if $count;

    return @closest;
  }
}

1;

=head1 NAME

Geo::Distance - Calculate Distances and Closest Locations

=head1 SYNOPSIS

  use Geo::Distance;
  
  my Geo::Distance $geo .= new;
  
  $geo.formula = 'hsin';
  $geo.reg_unit( 'toad_hop', 200120 );
  $geo.reg_unit( 'frog_hop', 6, 'toad_hop' );
  
  my $distance = $geo.distance( 'unit_type', $lon1,$lat1, $lon2,$lat2 );

  my @locations = (
    { lon => ..., lat => ... },
    { lon => ..., lat => ... },
    { lon => ..., lat => ... },
    ...,
  );
  my $locations = $geo.closest(
    $unit, $unit_count, $lon, $lat,
    :sort, :count(15)
  ) <== @locations;

=head1 DESCRIPTION

This perl library aims to provide as many tools to make it as simple as
possible to calculate distances between geographic points, and anything that
can be derived from that.  Currently there is support for finding the closest
locations within a specified distance, to find the closest number of points to
a specified point, and to do basic point-to-point distance calculations.

=head1 STABILITY

This is the first version of Geo::Distance to be considered to have a stable interface.  
You can now rely on the interface to be backwards compatible to version 0.07 and newer.

=head1 PROPERTIES

=head2 UNITS

All functions accept a unit type to do the computations of distance with.  By
default no units are defined in a Geo::Distance object.  You can add units with
reg_unit() or create some default units with default_units().

=head2 LATITUDE AND LONGITUDE

When a function needs a lon and lat they must always be in decimal degree
format.  Here is some sample code for converting from other formats to decimal:

  # DMS to Decimal
  my $decimal = $degrees + ($minutes/60) + ($seconds/3600);
  
  # Precision Six Integer to Decimal
  my $decimal = $integer * .000001;

If you want to convert from decimal radians to degrees you can use Math::Trig's
rad2deg function.

=head1 METHODS

=head2 new

  my Geo::Distance $geo .= new;
  my Geo::Distance $geo .= new(:no_units(1));

Returns a blessed Geo::Distance object.  The new constructor accepts one optional 
argument.

  no_unit - Whether or not to load the default units. Defaults to 0 (false).
            kilometer, meter, centimeter, yard, foot, inch, light_second, mile

=head2 formula

  if $geo.formula eq "hsin" { ... }
  $geo.formula = "cos";

Allows you to retrieve and set the formula that is currently being used to 
calculate distances.  The availabel formulas are hsin, polar, cos, and mt.  hsin 
is the default and mt/cos are depreciated in favor of hsin.  polar should be 
used when calculating coordinates near the poles.

=head2 reg_unit

  # Register 200,120 frog hops to travel the radius of the earth.
  $geo.reg_unit( 'toad_hop', 200120 );
  
  # For every toad hop there are 6 frog hops.
  $geo.reg_unit( 'frog_hop', 6, 'toad_hop' );

This method is used to create custom unit types.  There are two ways of calling it, 
depending on if you are defining the unit from scratch, or if you are basing it off 
of an existing unit (such as saying inches = feet / 12 ).  When defining a unit from 
scratch you pass the name and rho (radius of the earth in that unit) value.

So, if you wanted to do your calculations in human adult steps you would have to have an 
average human adult walk from the crust of the earth to the core (ignore the fact that 
this is impossible).  So, assuming we did this and we came up with 43,200 steps, you'd 
do something like the following.

  # Create adult_step unit.
  $geo.reg_unit( 'adult_step', 43200 );

Now, if you also wanted to do distances in baby steps you might think "well,
now I gotta get a baby to walk to the center of the earth".  But, you don't
have to!  If you do some research you'll find (no research was actually
conducted) that there are, on average, 4.7 baby steps in each adult step.

  # Create baby_step unit based off adult_step unit.
  $geo.reg_unit( 'baby_step' => 4.7 => 'adult_step' );

And if we were doing this in reverse and already had the baby step unit but not 
the adult step...

  # Create adult_step unit based off baby_step unit.
  $geo.reg_unit( 'adult_step', 1/4.7, 'baby_step' );

=head2 distance

  my $distance = $geo.distance( 'unit_type', $lon1,$lat1, $lon2,$lat2 );

Calculates the distance between two lon/lat points.

=head2 closest

This method finds the closest locations within a certain distance and returns
an hash reference of locations each with at least it's lon, lat, and the
distance.

  my @places = (
    { lon => ..., lat => ... },
    { lon => ..., lat => ... },
    { lon => ..., lat => ... },
    ...,
  );
  
  my $locations = $geo.closest(
    $unit, $distance,
    $lon, $lat,
    :sort, :count(10)
  ) <== @places;

  $unit - The name of the unit that you want the distances measured by.
  $distance - The number units out that you want to search.
  $lon, $lat - The longitutde and latitude.
  :sort - Sort the results
  :count(5) - Returns only the nearest 5 places

=head1 FORMULAS

=head2 hsin: Haversine Formula

  dlon = lon2 - lon1
  dlat = lat2 - lat1
  a = (sin(dlat/2))^2 + cos(lat1) * cos(lat2) * (sin(dlon/2))^2
  c = 2 * atan2( sqrt(a), sqrt(1-a) )
  d = R * c 

The hsin formula is the new standard formula for Geo::Distance because 
of it's improved accuracy over the cos formula.

=head2 polar: Polar Coordinate Flat-Earth Formula

  a = pi/2 - lat1
  b = pi/2 - lat2
  c = sqrt( a^2 + b^2 - 2 * a * b * cos(lon2 - lon1) )
  d = R * c 

While implimented, this formula has not been tested much.  If you use it 
PLEASE share your results with the author!

=head2 cos: Law of Cosines for Spherical Trigonometry

  a = sin(lat1) * sin(lat2)
  b = cos(lat1) * cos(lat2) * cos(lon2 - lon1)
  c = arccos(a + b)
  d = R * c

Although this formula is mathematically exact, it is unreliable for 
small distances because the inverse cosine is ill-conditioned.

=head2 mt: Math::Trig great_circle_distance

This formula uses Meth::Trig's great_circle_distance function which at this
time uses math almost exactly the same as the cos formula.  If you want to use
the cos formula you may find that mt will calculate faster (untested
assumption).  For some reason mt and cos return slight differences at very
close distances. The mt formula has the same drawbacks as the cos formula.

This is the same formula that was previously the only one used by 
Geo::Distance (ending at version 0.06) and was wrongly called the "gcd" formula.

Math::Trig states that the formula that it uses is:

  lat0 = 90 degrees - phi0
  lat1 = 90 degrees - phi1
  d = R * arccos(cos(lat0) * cos(lat1) * cos(lon1 - lon01) + sin(lat0) * sin(lat1))

=head1 TODO

=over 4

=item *

Test the polar formula.

=item *

Test the closest() function.  I've modified it since the last version but
haven't had a chance to test.

=item *

Berkely DB would be a nice alternative to DBI and Array closest() searching.

=item *

A second pass should be done in closest before distance calculations are made
that does an inner radius simplistic calculation to find the locations that are
obviously within the distance needed.

=item *

Tests!  We need tests!

=back

=head1 BUGS

Its probable since several of the parts mentioned in the TODO section have not
been tested.

Otherwise, none known right now, but by the time you read this, who knows?

=head1 CHEERS

Thanks!

=over 4

=item *

I<Dean Scott>

=item *

I<Michael R. Meuser>

=item *

I<Jack D.>

=item *

I<Bryce Nesbitt>

=back

=head1 AUTHOR

Copyright (C) 2005 Ingo Blechschmidt (port to Perl 6)

Copyright (C) 2003-2005 Aran Clary Deltac (CPAN: BLUEFEET)

This library is free software; you can redistribute it and/or modify
it under the same terms as Perl itself. 

Address bug reports and comments to: E<lt>aran@arandeltac.comE<gt>. When
sending bug reports, please provide the version of Geo::Distance, the version
of Perl, and the name and version of the operating system you are using.
Patches are welcome!

=head1 SEE ALSO

L<Math::Trig> - Inverse and hyperbolic trigonemetric Functions.

L<http://www.census.gov/cgi-bin/geo/gisfaq?Q5.1> - A overview of calculating distances.
