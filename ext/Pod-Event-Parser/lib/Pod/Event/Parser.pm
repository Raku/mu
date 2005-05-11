
use v6;
module Pod::Event::Parser-0.0.2;

my @EVENTS = (
    'start_document',
    'end_document',
    
    'start_element',
    'end_element',

    'start_modifier',
    'end_modifier',

    'verbatim',        
    'string',
    'newline'
);

sub parse (Str $filename, Hash %events is rw) returns Void is export {
    
    for @EVENTS -> $e { %events{$e} = sub { } unless ?%events{$e} }
    
    my $is_parsing = 0;
    my @for_or_begin;
    
    my $fh = open($filename);    
    loop {
        my $line = $fh.readline;
        last unless $line.defined; # exit as soon as possible
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
                        my $size = $0;
                        %events<start_element>('header', $size);
                        interpolate($1, %events);
                        %events<end_element>('header', $size);                       
                    }
                    when rx:perl5{^=over\s(\d)$} {
                        %events<start_element>('list', $0);
                    }
                    when rx:perl5{^=item\s(.*?)$} {
                        %events<start_element>('item');
                        interpolate($0, %events);
                        %events<end_element>('item');                     
                    }
                    when rx:perl5{^=back} {
                        %events<end_element>('list');
                    }
                    when rx:perl5{^=begin\s(.*?)$} {
                        push(@for_or_begin, 'begin');
                        %events<start_element>('begin', $0);
                    }                    
                    when rx:perl5{^=for\s(.*?)$} {
                        push(@for_or_begin, 'for');
                        %events<start_element>('for', $0);
                    }                    
                    when rx:perl5{^=end\s(.*?)$} {
                        my $last_for_or_begin = pop(@for_or_begin);
                        if ($last_for_or_begin eq 'for') {
                            %events<end_element>('for', $0);
                        }
                        else {
                            %events<end_element>('begin', $0);
                        }
                    }                    
                    when rx:perl5{^\s(.*?)$} {
                        my $verbatim = "$0\n";
                        my $_line = $fh.readline;
                        while (defined($_line)             && 
                              !($_line ~~ rx:perl5{^\n$})  && 
                                $_line ~~ rx:perl5{^\s(.*?)$} ) {                                
                            $verbatim ~= "$0\n";
                            $_line = $fh.readline;
                        }                        
                        %events<start_element>('verbatim');
                        %events<verbatim>($verbatim);
                        %events<end_element>('verbatim');                        
                    }
                    default {
                        if ($line ne '') {
                            %events<start_element>('paragraph');
                            interpolate($line, %events);   
                            my $_line = $fh.readline;
                            while (defined($_line)             && 
                                   !($_line ~~ rx:perl5{^\n$}) ) {
                                chomp($_line);
                                interpolate($_line, %events);
                                $_line = $fh.readline;
                            }                          
                            %events<end_element>('paragraph');
                        }
                        else {
                            %events<newline>();
                        }
                    }
                }
            } 
        }
    }
    $fh.close();
}

sub interpolate (Str $text, Hash %events) returns Void {    
    %events<start_element>('line_interpolation');
    # grab all the tokens with a split
	my @tokens = split(rx:perl5{([A-Z]<\s+|[A-Z]<|\s+>|>)}, $text);
    @tokens = @tokens.grep:{ defined($_) }; 
	# this is a memory stack for modifiers
	# it helps us track down problems
	my @modifier_stack;
	for (@tokens) -> $token {	
        given $token {
            when rx:perl5{^([A-Z])<$} {
                push(@modifier_stack, $0);
                %events<start_modifier>($0);
            }
            when rx:perl5{^>$} {
                # if we dont have anything on the stack
                # then we are not balanced, and should die
                (+@modifier_stack) || die "Unbalanced close modifier (>) found";
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
    %events<end_element>('line_interpolation');        
}

=pod

=head1 NAME

Pod::Event::Parser - A simple event based POD parser

=head1 SYNOPSIS

  use v6;
  use Pod::Event::Parser;
  
  parse("path/to/file.pod", %event_handlers);

=head1 DESCRIPTION

This is a straightforward event based POD parser, it is modeled after XML SAX 
parsers and is currently in the early stages of development.   

=head1 LIMITATIONS & CAVEATS

Basic parsing functionality is fairly solid right now. The F<t/complex.t> test 
actually roundtrip parses this document successfully. However, more complex POD 
documents may not do as well. Error reporting is still pretty minimal and the 
parser is not very forgiving about whitespace in some situations. 

=head1 FUNCTIONS

=over 4

=item B<parse (Str $filename, Hash %events)>

The main C<parse> function currently takes a C<$filename> and a Hash of C<%events> 
which it uses to process each line of the file.

=back

=head1 EVENTS

=head2 Document Events

=over 4

=item I<start_document>

This event is fired when a '=pod' directive is reached.

=item I<end_document>

This event is fired when a '=cut' directive is reached.

=back

=head2 Element Events

For each POD element, an event is fired, the first argument to all these events is
the C<$event_type> (header, list, item, etc.). 

=over 4

=item I<start_element ('header', $size)>

This event is fired once the '=head(n)' directive is reached. This does not include 
the text of the header, that will be processed as a interpolated string (to allow for
any modifiers).

=item I<end_element ('header')>

This event is fired after the '=head(n)' directive and it's accompanying text has been
parsed.

=item I<start_element ('begin')>

This event is fired when a '=begin' directive is reached.

=item I<end_element ('begin')>

This event is fired when a '=end' directive (which is preceeded by an '=begin' directive) 
is reached.

=item I<start_element ('for')>

This event is fired when a '=for' directive is reached.

=item I<end_element ('for')>

This event is fired when a '=end' directive (which is preceeded by an '=for' directive) 
is reached.    
    
=item I<start_element ('list')>

This event is fired when an '=over' directive is reached.

=item I<end_element ('list')>

This event is fired when a '=back' directive is reached.
    
=item I<start_element ('item')>

This event is fired when an '=item' directive is reached, the next event fired will be
any text which follows the item.

=item I<end_element ('item')>

This event is fired once the last '=item' directive, followed by any text on the same line.

=item I<start_element ('verbatim')>

This event is fired once a verbatim section is found.
    
=item I<end_element ('verbatim')>

This event closes a verbatim section.

=item I<start_element ('line_interpolation')>

This event is begun when any block of characters is interpolated for modifiers.

=item I<end_element ('line_interpolation')>

This event is fired when interpolation is complete.

=back

=head2 Modifier Events

=over 4

=item I<start_modifier>

This event is fired when a modifier is encountered.

=item I<end_modifier>

This event closes a modifier.

=back

=head2 Text Events

=over 4

=item I<verbatim>

This event handles the entire verbatim string.

=item I<string>

This event processes raw strings.

=item I<newline>

This event handles newlines in the text, it is a specialization of the 'string' event.

=back

=head1 SEE ALSO

=over 4

=item Any of the Perl5 POD parsers

=item XML::SAX

=back

=head1 AUTHOR

stevan little, E<lt>stevan@iinteractive.comE<gt>

=head1 COPYRIGHT

Copyright (c) 2005. Stevan Little. All rights reserved.

This program is free software; you can redistribute it and/or modify
it under the same terms as Perl itself.

See L<http://www.perl.com/perl/misc/Artistic.html>

=cut
