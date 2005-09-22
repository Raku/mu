#!/usr/bin/pugs

use v6;
use Test;

# Tests testing that automatical referentiation (e.g. $arrayref = @array)
# works.

plan 8;

# Implicit referentiation of arrays in assignment
{
    my @array = <a b c>;
    my $ref   = @array;

    is ~$ref, "a b c", '$arrayref = @array works';
}

# Explicit referentiation of arrays in assignment
{
    my @array = <a b c>;
    my $ref   = \@array;

    is ~$ref, "a b c", '$arrayref = \@array works';
}

# Implicit referentiation of hashes in assignment
{
    my %hash = (a => 1, b => 2, c => 3);
    my $ref  = %hash;

    is ~$ref.values.sort, "1 2 3", '$hashref = %hash works';
}

# Explicit referentiation of hashes in assignment
{
    my %hash = (a => 1, b => 2, c => 3);
    my $ref  = \%hash;

    is ~$ref.values.sort, "1 2 3", '$hashref = \%hash works';
}

# Implicit referentiation of arrays in pair creation
{
    my @array = <a b c>;
    my $pair  = (key => @array);

    is ~$pair.value, "a b c", '(key => @array) works';
}

# Explicit referentiation of arrays in pair creation
{
    my @array = <a b c>;
    my $pair  = (key => \@array);

    is ~$pair.value, "a b c", '(key => \@array) works';
}

# Implicit referentiation of hashes in pair creation
{
    my %hash = (a => 1, b => 2, c => 3);
    my $pair = (key => %hash);

    is ~$pair.value.values.sort, "1 2 3", '(key => %hash) works';
}

# Explicit referentiation of hashes in pair creation
{
    my %hash = (a => 1, b => 2, c => 3);
    my $pair = (key => \%hash);

    is ~$pair.value.values.sort, "1 2 3", '(key => \%hash) works';
}
