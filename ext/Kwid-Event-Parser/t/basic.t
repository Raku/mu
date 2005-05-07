#!/usr/bin/pugs

use v6;
use Test;
use File::Spec;

plan 1;

use Kwid::Event::Parser;

my $buffer = "";

my %events = (
    # Elements
    start_element => -> ($event_type, @args) { 
        given $event_type {
            when 'header' {
                my $size = @args.shift;
                $buffer ~= "<H$size>";          
            }
            when 'list' {
                $buffer ~= "<UL>\n";
            }
            when 'item' {
                $buffer ~= "<LI>";
            }
            when 'paragraph' {
                $buffer ~= "<P>" 
            }          
            when 'verbatim' {
                $buffer ~= "<PRE>\n" 
            }                                                                                                                                      
        }
    },
    end_element => -> ($event_type, @args) { 
        given $event_type {
            when 'header' {
                my $size = @args.shift;
                $buffer ~= "</H$size>\n";                     
            }
            when 'list' {
                $buffer ~= "</UL>\n"
            }  
            when 'item' {
                $buffer ~= "</LI>\n";
            }                
            when 'paragraph' {
                $buffer ~= "</P>\n" 
            }     
            when 'verbatim' {
                $buffer ~= "</PRE>\n" 
            }                                                       
        }
    },          

    # Modifiers
    start_modifier => -> ($mod) { 
        given $mod {
            when "`" {
                $buffer ~= "<CODE>"                     
            }
            when '*' {
                $buffer ~= "<B>"             
            }
            when '/' {
                $buffer ~= "<I>"             
            }            
        }
    },
    end_modifier   => -> ($mod) { 
        given $mod {
            when "`" {
                $buffer ~= "</CODE>"                     
            }
            when '*' {
                $buffer ~= "</B>"             
            }
            when '/' {
                $buffer ~= "</I>"             
            } 
        }
    },      

    # Text handling
    verbatim => -> ($text) { 
        my @lines = split("\n", $text); 
        for (@lines) -> $line {
            if ($line eq '') {
                $buffer ~= "\n";
            }
            else {
                $buffer ~= " $line\n";                                            
            }
        }
        # trim the last newline
        chomp($buffer);
    },
    string  => -> ($str)  { $buffer ~= $str  }   
);

lives_ok {
    parse(catfile('t', 'sample.kwid'), %events);
}, '... we parsed the sample file without die-ing';

diag $buffer;
