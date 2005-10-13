#!/usr/bin/pugs

use v6;
use Test;

# Tests testing that automatical referentiation (e.g. $arrayref = @array)
# works.

plan 42;

# Implicit referentiation of arrays in assignment
{
    my @array = <a b c>;
    my $ref   = @array;

    is ~$ref, "a b c", '$arrayref = @array works (1)';
    is +$ref,       3, '$arrayref = @array works (2)';
}

# Explicit referentiation of arrays in assignment
{
    my @array = <a b c>;
    my $ref   = \@array;

    is ~$ref, "a b c", '$arrayref = \@array works (1)';
    is +$ref,       3, '$arrayref = \@array works (2)';
}

# Implicit referentiation of hashes in assignment
{
    my %hash = (a => 1, b => 2, c => 3);
    my $ref  = %hash;

    is ~$ref.values.sort, "1 2 3", '$hashref = %hash works (1)';
    is +$ref.values,            3, '$hashref = %hash works (2)';
}

# Explicit referentiation of hashes in assignment
{
    my %hash = (a => 1, b => 2, c => 3);
    my $ref  = \%hash;

    is ~$ref.values.sort, "1 2 3", '$hashref = \%hash works (1)';
    is +$ref.values,            3, '$hashref = \%hash works (2)';
}

# Implicit referentiation of arrays in assignment to an array element
{
    my @array = <a b c>;
    my @other;
    @other[1] = @array;

    is ~@other,    " a b c", '@other[$idx] = @array works (1)';
    is +@other,           2, '@other[$idx] = @array works (2)';
    is +@other[1],        3, '@other[$idx] = @array works (3)';
}

# Explicit referentiation of arrays in assignment to an array element
{
    my @array = <a b c>;
    my @other;
    @other[1] = \@array;

    is ~@other, " a b c", '@other[$idx] = \@array works (1)';
    is +@other,        2, '@other[$idx] = \@array works (2)';
    is +@other[1],     3, '@other[$idx] = \@array works (3)';
}

# Implicit referentiation of hashes in assignment to an array element
{
    my %hash = (a => 1, b => 2, c => 3);
    my @other;
    @other[1] = %hash;

    is +@other,    2, '@other[$idx] = %hash works (1)';
    is +@other[1], 3, '@other[$idx] = %hash works (2)';
}

# Explicit referentiation of hashes in assignment to an array element
{
    my %hash = (a => 1, b => 2, c => 3);
    my @other;
    @other[1] = \%hash;

    is +@other,    2, '@other[$idx] = \%hash works (1)';
    is +@other[1], 3, '@other[$idx] = \%hash works (2)';
}

# Implicit referentiation of arrays in assignment to a hash element
{
    my @array = <a b c>;
    my %other;
    %other<a> = @array;

    is +%other,    1, '%other[$key] = @array works (1)';
    is +%other<a>, 3, '%other[$key] = @array works (2)';
}

# Explicit referentiation of arrays in assignment to a hash element
{
    my @array = <a b c>;
    my %other;
    %other<a> = \@array;

    is +%other,    1, '%other[$key] = \@array works (1)';
    is +%other<a>, 3, '%other[$key] = \@array works (2)';
}

# Implicit referentiation of hashes in assignment to a hash element
{
    my %hash = (a => 1, b => 2, c => 3);
    my %other;
    %other<a> = %hash;

    is +%other,    1, '%other[$key] = %hash works (1)';
    is +%other<a>, 3, '%other[$key] = %hash works (2)';
}

# Explicit referentiation of hashes in assignment to a hash element
{
    my %hash = (a => 1, b => 2, c => 3);
    my %other;
    %other<a> = \%hash;

    is +%other,    1, '%other[$key] = \%hash works (1)';
    is +%other<a>, 3, '%other[$key] = \%hash works (2)';
}

# Implicit referentiation of arrays in pair creation with key => ...
{
    my @array = <a b c>;
    my $pair  = (key => @array);

    is ~$pair.value, "a b c", '(key => @array) works (1)';
    is +$pair.value,       3, '(key => @array) works (2)';
}

# Explicit referentiation of arrays in pair creation with key => ...
{
    my @array = <a b c>;
    my $pair  = (key => \@array);

    is ~$pair.value, "a b c", '(key => \@array) works (1)';
    is +$pair.value,       3, '(key => \@array) works (2)';
}

# Implicit referentiation of hashes in pair creation with key => ...
{
    my %hash = (a => 1, b => 2, c => 3);
    my $pair = (key => %hash);

    is ~$pair.value.values.sort, "1 2 3", '(key => %hash) works (1)';
    is +$pair.value.values,            3, '(key => %hash) works (2)';
}

# Explicit referentiation of hashes in pair creation with key => ...
{
    my %hash = (a => 1, b => 2, c => 3);
    my $pair = (key => \%hash);

    is ~$pair.value.values.sort, "1 2 3", '(key => \%hash) works (1)';
    is +$pair.value.values,            3, '(key => \%hash) works (2)';
}

# Implicit referentiation of arrays in pair creation with :key(...)
{
    my @array = <a b c>;
    my $pair  = (:key(@array));

    is ~$pair.value, "a b c", '(:key(@array)) works (1)';
    is +$pair.value,       3, '(:key(@array)) works (2)';
}

# Explicit referentiation of arrays in pair creation with :key(...)
{
    my @array = <a b c>;
    my $pair  = (:key(\@array));

    is ~$pair.value, "a b c", '(:key(\@array)) works (1)';
    is +$pair.value,       3, '(:key(\@array)) works (2)';
}

# Implicit referentiation of hashes in pair creation with :key(...)
{
    my %hash = (a => 1, b => 2, c => 3);
    my $pair = (:key(%hash));

    is ~$pair.value.values.sort, "1 2 3", '(:key(%hash)) works (1)';
    is +$pair.value.values,            3, '(:key(%hash)) works (2)';
}

# Explicit referentiation of hashes in pair creation with :key(...)
{
    my %hash = (a => 1, b => 2, c => 3);
    my $pair = (:key(\%hash));

    is ~$pair.value.values.sort, "1 2 3", '(:key(\%hash)) works (1)';
    is +$pair.value.values,            3, '(:key(\%hash)) works (2)';
}
