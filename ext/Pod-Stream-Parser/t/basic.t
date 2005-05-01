#!/usr/bin/pugs

use v6;
require Test;

require Pod::Stream::Parser;
require File::Spec;

my $test_output;

my %events = (
    start_document         => { $test_output ~= "=pod\n\n" },
    end_document           => { $test_output ~= "=cut" },
    start_header           => -> ($size) { $test_output ~= "=head$size " },
    end_header             => { $test_output ~= "\n" },    
    start_list             => -> ($indent) { $test_output ~= "=over $indent\n\n" },
    end_list               => { $test_output ~= "=back\n\n" },  
    start_item             => { $test_output ~= "=item " },
    end_item               => { $test_output ~= "\n" },    
    verbatim               => -> ($text) { 
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
    end_paragraph          => { $test_output ~= "\n" },
    start_modifier         => -> ($mod) { $test_output ~= $mod ~ "<" },
    end_modifier           => -> ($mod) { $test_output ~= ">" },
    string                 => -> ($str) { $test_output ~= $str },
    end_line_interpolation => { $test_output ~= "\n" }       
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
