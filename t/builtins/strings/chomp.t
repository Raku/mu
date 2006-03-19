#!/usr/bin/pugs

use v6;
use Test;

plan 16;

=pod

Basic tests for the chomp() builtin

=cut

# L<S29/"Perl6::Str" /chomp/>
# Also see L<"http://use.perl.org/~autrijus/journal/25351">
#   &chomp and &wrap are now nondestructive; chomp returns the chomped part,
#   which can be defined by the filehandle that obtains the default string at
#   the first place. To get destructive behaviour, use the .= form.

{
    my $foo = "foo\n";
    chomp($foo);
    is($foo, "foo\n", 'our variable was not yet chomped');
    $foo .= chomp;
    is($foo, 'foo', 'our variable is chomped correctly');
    $foo .= chomp;
    is($foo, 'foo', 'our variable is chomped again with no effect');
}

{
    my $foo = "foo\n\n";
    $foo .= chomp;
    is($foo, "foo\n", 'our variable is chomped correctly');
    $foo .= chomp;
    is($foo, 'foo', 'our variable is chomped again correctly');
    $foo .= chomp;
    is($foo, 'foo', 'our variable is chomped again with no effect');
}

{
    my $foo = "foo\nbar\n";
    $foo .= chomp;
    is($foo, "foo\nbar", 'our variable is chomped correctly');
    $foo .= chomp;
    is($foo, "foo\nbar", 'our variable is chomped again with no effect');
}

{
    my $foo = "foo\n ";
    $foo .= chomp;
    is($foo, "foo\n ", 'our variable is chomped with no effect');
}

{
    my $foo = "foo\n";
    my $chomped_foo = try { chomp($foo).newline };
    is($chomped_foo, "\n", 'chomp(...).newline returns the chomped value', :todo<feature>);
    is($foo, "foo\n", 'and our variable was not chomped');
}

{
    my $foo = "foo\n";
    $foo .= chomp;
    my $chomped_foo = try { $foo.newline };
    is($chomped_foo, "\n", 'chomp(...).newline returns the chomped value', :todo<feature>);
    is($foo, "foo", 'and our variable was chomped');
}

{
    my $foo = "foo\n\n";
    my $chomped = $foo.chomp;
    is($foo, "foo\n\n", ".chomp has no effect on the original string");
    is($chomped, "foo\n", ".chomp returns correctly chomped value");
    
    # $chomped.chomp.newline

    $chomped = $chomped.chomp;
    is($chomped, "foo", ".chomp returns correctly chomped value again");
}

