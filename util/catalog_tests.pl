#!/usr/bin/perl -w

use strict;
use Fatal qw/open close opendir closedir/;
use File::Spec::Functions;

$\ = "\n\n";

sub sort_files {
    return ((-d $a && -f $b) ? 1 :       # file beats directory
            (-d $a && -f $b) ? -1 :      # file beats directory
                (lc($a) cmp lc($b)))     # otherwise just sort 'em
}

sub list_dir {
	my ($path) = @_;
	opendir DIR, $path;
	my @contents = map { catfile($path, $_) } grep {
					$_ ne curdir() && $_ ne updir()
					} readdir(DIR);
	close DIR;
	return sort "sort_files", @contents;
}

sub catalog_tests {
    my ($dir) = @_;
    map {
        if (-d $_ && !/Synopsis/ && !/Dialects/ && !/\.svn/) {
        	print "=cut";
        	print "### SPLIT HERE ### $_ ###"; # to be munged by F<util/split_test_catalog.pl> (you can just pipe)
        	print "=pod";
        	
            print "=item F<$_>";
            print "=over 4";
            catalog_tests($_);
            print "=back";
        }
        elsif (-f $_ && /\.t$/) {
            print "=item F<$_>";
            catalog_file($_);    
        }
    } list_dir($dir);
}

sub catalog_file {
    my ($file) = @_;
    my $todo_tests = 0;
    my $documenation = '';
    my $tests_planned = 0;
    my $in_doc;
    my @tests;
    open FH, "<", $file;
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
        if (/( # the whole subname
        	(todo_)?  # can be todo
        	(ok|is|fail|isa_ok) # must be one of these
        )/x){
			my ($test_sub, $todo, $test_type) = ($1, $2, $3);
			my ($desc, $comment);
			
			if (/
				.* # stuff # shit. sorry.
				\,\s* # item
				['"](.*)['"] # some quoted text
			/x){ $desc = $1 }
			
			if (/
				;\s* # ending the statement, and from there on whitespace
				\#\s*(.*)$ # followed by a comment
			/x){ $comment = $1 }
		
	        $todo&&=1==1; # booleanize ;-
	        
        	$todo_tests++ if $todo;
        	
        	push @tests, {
        		line => $.,
        		type => $test_type,
        		desc => $desc,
        		comment => $comment,
        		todo => $todo,
        		# code => $_,
        	};
        }
    }
    close FH;
    print "Test planned: $tests_planned";
    print "Test TODO: $todo_tests" if $todo_tests;
    print "$documenation";

    
    if (@tests){
    	print "=item Test cases";
    	print "=over 4";

		foreach my $test (@tests){
			print "=item line $test->{line}";
			
			#print "\t$test->{perl}" if $ARGV[0]; # only print SLOC if true arg was passed
			
			print "desc: $test->{desc}" if $test->{desc};
			print "type: $test->{type}()" . ($test->{todo} ? " # TODO" : "");
			print "comment: $test->{comment}" if $test->{comment};
		}
		
		print "=back";
	}
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
    b) the Artistic License, version 2.0beta5 or any later version.

For the full license text, please see the F<GPL-2> and F<Artistic-2> files
under the F<LICENSE> directory in the Pugs distribution.

=cut

};
