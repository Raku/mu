#!/usr/bin/pugs

use v6;
use Test;
use File::Spec;

plan 5;

use_ok('Pod::Event::Parser');
use_ok('Pod::Event::Handler::POD');
try { chdir "ext/Pod-Event-Parser" }; # Hack if we're run from make smoke

my $test_output = "";

parse(catfile('t', 'sample.pod'), pod2pod($test_output));
is($test_output, 
'=pod

=head1 This is B<header> 1

=over 4

=item This is an I<item>

This is the B<items> body

=back

=head2 This is header 2

This is regular text which 
wraps I<up B<to>> two lines

 This is verbatim text
 which contains some code in it
 for () {
     this is the stuff
 }

This is regular text again

=cut',
'... POD round-tripping worked');

# now try it using the filehandle

my $fh = open('test.pod', :w);
parse(catfile('t', 'sample.pod'), pod2pod($fh));
$fh.close();

my $file_contents = slurp('test.pod');
is($file_contents, 
'=pod

=head1 This is B<header> 1

=over 4

=item This is an I<item>

This is the B<items> body

=back

=head2 This is header 2

This is regular text which 
wraps I<up B<to>> two lines

 This is verbatim text
 which contains some code in it
 for () {
     this is the stuff
 }

This is regular text again

=cut',
'... POD round-tripping worked (with file-handle)');

ok((unlink('test.pod') && !-e 'test.pod'), '... removing the temp POD file, and making sure it is really gone');
