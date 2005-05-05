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
    string => -> ($str) { $test_output ~= $str }         
);

my $file_path = catfile('lib', 'Pod', 'Event', 'Parser.pm');

my $expected_output;
my $start = 0;
my $fh = open($file_path);
for =$fh -> $line {
    $start = 1 if $line ~~ rx:perl5/^=pod/;
    if $start { $expected_output ~= $line }    
    $start = 0 if $line ~~ rx:perl5/^=cut/;    
}
$fh.close();
chomp($expected_output);

parse($file_path, %events);
#diag $test_output;
is($test_output, $expected_output, '... complex POD document parsed correctly');

