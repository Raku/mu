use v6;

module Pod::Stream::Parser-0.0.1;

my @EVENTS = (
    'start_document',
    'end_document',
    
    'start_header',
    'end_header',

    'start_begin',
    'end_begin',

    'start_for',
    'end_for',
    
    'start_list',
    'end_list',
    
    'start_item',
    'end_item',
    
    'verbatim',
    
    'start_interpolation',
    'end_interpolation',
        
    'start_modifier',
    'end_modifier',
    'string',
    
    'newline'
);

sub parse (Str $filename, Hash %_events) is export {
    my %events = %_events; # << we have to do this, %_events is immutable
    for (@EVENTS) -> $e { %events{$e} = sub { '' } unless ?%events{$e} }
    my $fh = open($filename);    
    my $is_parsing = 0;
    my @for_or_begin;
    for (=$fh) -> $_line {
        my $line = $_line; # << we have to do this, $_line is immutable
        chomp($line);
        if ($line ~~ rx:perl5{^=pod}) {
            $is_parsing = 1;
            %events<start_document>();
        }
        else {
            if ($is_parsing) {
                given $line {
                    when rx:perl5{^=cut} { 
                        $is_parsing = 0;
                        %events<end_document>();                    
                    }
                    when rx:perl5{^=head(\d)\s(.*?)$} {
                        my $size = $1;
                        %events<start_header>($size);
                        interpolate($2, %events);
                        %events<end_header>($size);                       
                    }
                    when rx:perl5{^=over\s(\d)$} {
                        %events<start_list>($1);
                    }
                    when rx:perl5{^=item\s(.*?)$} {
                        %events<start_item>();
                        interpolate($1, %events);
                        %events<end_item>();                     
                    }
                    when rx:perl5{^=back} {
                        %events<end_list>();
                    }
                    when rx:perl5{^=begin\s(.*?)$} {
                        push(@for_or_begin, 'begin');
                        %events<start_begin>($1);
                    }                    
                    when rx:perl5{^=for\s(.*?)$} {
                        push(@for_or_begin, 'for');
                        %events<start_for>($1);
                    }                    
                    when rx:perl5{^=end\s(.*?)$} {
                        my $last_for_or_begin = pop(@for_or_begin);
                        if ($last_for_or_begin eq 'for') {
                            %events<end_for>($1);
                        }
                        else {
                            %events<end_begin>($1);
                        }
                    }                    
                    when rx:perl5{^\s(.*?)$} {
                        %events<verbatim>($1);
                    }
                    default {
                        interpolate($line, %events);   
                        %events<newline>() unless $line eq '';
                    }
                }
            } 
        }
    }
    $fh.close();
}

sub interpolate (Str $text, Hash %events) {
    %events<string>($text); # <<< this is a HACK for now
    
    %events<start_interpolation>();
	my @tokens = (); # $text ~~ rx:perl5:g{(?:[A-Z]<\s+|[A-Z]<|\s+>|>|\w+|\s+)};
	# this is a memory stack for modifiers
	# it helps up track down problems
	my @modifier_stack;
	for (@tokens) -> $token {	
        given $token {
            when rx:perl5{^([A-Z])<$} {
                push(@modifier_stack, $1);
                %events<start_modifier>($1);
            }
            when rx:perl5{^>$} {
                # if we dont have anything on the stack
                # then we are not balanced, and should die
                (!@modifier_stack) || die "Unbalanced close modifier (>) found";
                # if we have one the stack ..
                # then this one probably matches,
                # so we can pop that modifier off  
                my $last_mod = pop(@modifier_stack);
                %events<end_modifier>($last_mod);
            }
            default {
                %events<string>($token);
            }
        }
	}
	# if we find we still have some modifiers
	# on the stack, then we have an error, so 
	# we need to throw an exception about it.
	(!@modifier_stack) 
		|| die "Unbalanced modifier(s) found (" ~ join(", ", @modifier_stack) ~ ") in the string (@tokens)";
    %events<end_interpolation>();
}

=pod

=head1 NAME

Pod::Stream::Parser - A simple stream based POD parser

=head1 SYNOPSIS

  use v6;
  require Pod::Stream::Parser;
  
  parse("path/to/file.pod", %event_handlers);

=head1 DESCRIPTION

This is a very simple stream based POD parser, it is modeled after SAX style parsers
and is currently still in the very early stages of development. 

=head1 LIMITATIONS & CAVEATS

Functionality is B<severaly> limited right now. See the tests for more details.

=head1 FUNCTIONS

=over 4

=item B<parse (Str $filename, Hash %events)>

The main C<parse> function currently takes a C<$filename> and a Hash of C<%events> 
which it uses to process each line of the file.

=back

=head1 EVENTS

=over 4

=item I<start_document>

=item I<end_document>
    
=item I<start_header>

=item I<end_header>

=item I<start_begin>

=item I<end_begin>

=item I<start_for>

=item I<end_for>
    
=item I<start_list>

=item I<end_list>
    
=item I<start_item>

=item I<end_item>
    
=item I<verbatim>
    
=item I<start_interpolation>

=item I<end_interpolation>
        
=item I<start_modifier>

=item I<end_modifier>

=item I<string>

=item I<newline>

=back

=head1 SEE ALSO

Any of the Perl5 POD parsers

=head1 AUTHOR

stevan little, E<lt>stevan@iinteractive.comE<gt>

=head1 COPYRIGHT

Copyright (c) 2005. Stevan Little. All rights reserved.

This program is free software; you can redistribute it and/or modify
it under the same terms as Perl itself.

See http://www.perl.com/perl/misc/Artistic.html

=cut
