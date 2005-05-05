#!/usr/bin/pugs

use v6;
use Test;

use Pod::Event::Parser;
use File::Spec;

my $test_output;

my %events = (
    # Documents
    start_document => { $test_output ~= "=pod\n\n" },
    end_document   => { $test_output ~= "=cut"     },
    
    # Elements
    start_element => -> ($event_type, @args) { 
        given $event_type {
            when 'header' {
                my $size = @args.shift;
                $test_output ~= "=head$size ";          
            }
            when 'list' {
                my $indent = @args.shift;
                $test_output ~= "=over $indent\n\n";
            }
            when 'item' {
                $test_output ~= "=item ";
            }
        }
    },
    end_element => -> ($event_type, @args) { 
        given $event_type {
            when 'list' {
                $test_output ~= "=back\n\n"
            }  
            when rx:perl5/^header|item|paragraph|line_interpolation$/ {
                $test_output ~= "\n" 
            }              
        }
    },          
      
    # Modifiers
    start_modifier => -> ($mod) { $test_output ~= $mod ~ "<" },
    end_modifier   => -> ($mod) { $test_output ~= ">" },      

    # Text handling
    verbatim => -> ($text) { 
        my @lines = split("\n", $text); 
        for (@lines) -> $line {
            if ($line eq '') {
                $test_output ~= "\n";
            }
            else {
                $test_output ~= " $line\n";                                            
            }

        }
    },
    string => -> ($str) { $test_output ~= $str },                                
);

parse(catfile('t', 'sample.pod'), %events);
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
