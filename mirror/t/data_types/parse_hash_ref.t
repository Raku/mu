#!/usr/bin/pugs

use v6;
use Test;

=kwid

Problem assigning a hash ref as a value to a key when creating hash

=cut

plan 8;

# The problem only seems to present itself when you are initializing the hash
# If you initialize and then assign the value to a hash key it works as expected


{
    my %h1;
    my $h_ref = \%h1;
    is(ref $h_ref, 'Hash', "Simple hash reference works correctly");

    my %h2;
    %h2<key> = $h_ref;
    is(ref %h2<key>, 'Hash', "Using existing reference as hash works if not creating hash");

    my %h3;
    %h3<key> = \%h1;
    is(ref %h3<key>, 'Hash', "Creating a new reference to hash works if not creating hash");

    my %h4 = ('key' => $h_ref);
    is(ref %h4<key>, 'Hash', "Using existing reference breaks when creating hash", :todo<bug>);

    my %h5 = ('key' => \%h1);
    is(ref %h5<key>, 'Hash', "Creating a new reference breaks when creating hash", :todo<bug>);
}

{
    my %h1 = (k1 => 'v1');
    my $h_ref = \%h1;

    my %h2 = ('key' => $h_ref);
    is(ref %h2<key>, 'Hash', "Using existing reference (w/ values) breaks when creating hash", :todo<bug>);

    my %h3 = ('key' => \%h1);
    is(ref %h3<key>, 'Hash', "Creating a new reference (w/ values) breaks when creating hash", :todo<bug>);
}

{
    my %h1;
    my %h2;
    
    my %h3 = ('h1' => \%h1, 'h2' => \%h2);

    is(ref %h3<h1>, 'Hash', "Creating hash with multiple nested hashrefs (w/o values)", :todo<bug>);
}
