#!/usr/bin/perl6

use v6;

=head1 opening a file

You want to read or write a file from Perl.

=cut

my $path = "07-01opening_file.p6";

my $input = open($path, :r)
	err die "Could not open $path for reading $!\n";

my $filename = "test_file";
my $output = open($filename, :w) 
	err die "Could not open $filename for writing $!\n";


# File-access modes
# If you want to use any of the mode parameters you have to put parenthese () after the open.
# :r      read, this is the defaul open mode
# :w      write, automatically creating non existing files and emptying existing files
# :a      append, keep the file intact and enable to write at the end of it
# :rw     read and write
# :r, :w  
# :ra     read and append
# :r, :a  
#      write and read (overwriting a a file) will we have this?

# Closing the file
# $input.close err die $!;
# close($input);


# $output.print "some string\n";
# print $output, "String\n";


# readline($input);
# $input.readline
# =$input

# sysopen ?
 


