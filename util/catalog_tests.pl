#!/usr/bin/perl -w

use strict;

sub sort_files { 
    my ($left, $right) = @_;   
    return ((-d $left && -f $right) ? 1 :       # file beats directory
            (-d $right && -f $left) ? -1 :      # file beats directory
                (lc($left) cmp lc($right)))     # otherwise just sort 'em
}

sub catalog_tests {
    my ($dir, $depth) = @_;
    $depth ||= 1;
    map { 
        if (-d $_ && !/Synopsis/ && !/Dialects/) {
            print "=item B<$_>\n\n";
            print "=over 4\n\n";     
            catalog_tests($_, $depth + 1);
            print "=back\n\n";
        }
        elsif (-f $_) {
            print "=item I<$_>\n\n";
            catalog_file($_);    
        }
    } sort { sort_files($a, $b) } <$dir/*>;
}

sub catalog_file {
    my ($file) = @_;
    my $todo_tests = 0;
    my $documenation = '';
    my $tests_planned = 0;
    my $in_doc;
    open(FH, "<", $file);
    while (<FH>) {
        chomp();
        if (/=pod/ || /=kwid/) {
            $in_doc = 1;
        }
        elsif (/=cut/) {
            $in_doc = 0;
        }
        elsif ($in_doc) {
            $documenation .= "  $_\n" unless !$_ || /^\s+$/ || /^\=/;
        }
        elsif (/^plan.*?(\d+)/) {
            $tests_planned = $1;
        }
        $todo_tests++ if /todo_(ok|is|fail|isa_ok)/;
    }
    close FH;
    print "Test planned: $tests_planned\n\n";
    print "Test TODO: $todo_tests\n\n" if $todo_tests;
    print "$documenation\n";
}

print q{
=pod

=head1 NAME

Pugs Test Catalog

=head1 DESCRIPTION

This is an automatically generated file of all the tests in the Pugs 
test suite. It also attempts to extract any additional information it
can out of each test file.

The purpose of this document is to help people who want to get involved
in Pugs.

=head1 TEST FILES

=over 4

};

catalog_tests('t');

print q{

=back

=head1 COPYRIGHT

Copyright 2005 by Autrijus Tang E<lt>autrijus@autrijus.orgE<gt>.

This code is free software; you can redistribute it and/or modify it under
the terms of either:

    a) the GNU General Public License, version 2, or
    b) the Artistic License, version 2.0beta5.

For the full license text, please see the F<GPL-2> and F<Artistic-2> files
under the F<LICENSE> directory in the Pugs distribution.

=cut

};