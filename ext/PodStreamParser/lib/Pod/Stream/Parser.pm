use v6;

module Pod::Stream::Parser-0.0.1;

sub parse (Str $filename, Hash %events) is export {
    my $fh = open($filename);
    my $parsing = 0;
    for (=$fh) -> $_line {
        my $line = $_line; # << we have to do this, $_line is immutable
        chomp($line);
        if ($line ~~ rx:perl5{^=pod}) {
            $parsing = 1;
            %events<begin_document>();
        }
        else {
            if ($parsing) {
                given $line {
                    when rx:perl5{^=cut} { 
                        $parsing = 0;
                        %events<end_document>();                    
                    }
                    when rx:perl5{^=head(\d)\s(.*?)$} {
                        my $size = $1;
                        %events<begin_header>($size);
                        interpolate($2, %events);
                        %events<end_header>($size);                       
                    }
                    when rx:perl5{^=over\s(\d)$} {
                        %events<begin_list>($1);
                    }
                    when rx:perl5{^=item\s(.*?)$} {
                        %events<begin_item>();
                        interpolate($1, %events);
                        %events<end_item>();                     
                    }
                    when rx:perl5{^=back} {
                        %events<end_list>();
                    }
                    when rx:perl5{^\s+(.*?)$} {
                        %events<verbatim>($1);
                    }
                    default {
                        if ($line ~~ rx:perl5{\<}) {
                            interpolate($line, %events);    
                        }
                        else {
                            %events<text>($line);
                        }                    
                    }
                }
            } 
        }
    }
    $fh.close();
}

sub interpolate (Str $text, Hash %events) {
    %events<string>($text);
    # I need either split(<regexp>) support or 
    # the ability to capture matches  
# 	my @tokens = $text ~~ rx:perl5:g{(?:[A-Z]<\s+|[A-Z]<|\s+>|>|\w+|\s+)};
# 	# this is a memory stack for modifiers
# 	# it helps up track down problems
# 	my @modifier_stack;
# 	for (@tokens) -> $token {	
# 		if ($token ~~ rx:perl5{([A-Z])<}) {
# 			push(@modifier_stack, $1);
#             %events<begin_modifier>($1);
# 		}
# 		elsif ($token ~~ rx:perl5{\>}) {
# 			# if we have one the stack ..
# 			if (+@modifier_stack) {
# 				# then this one probably matches,
# 				# so we can pop that modifier off  
# 				my $last_mod = pop(@modifier_stack);
#                 %events<end_modifier>($last_mod);
# 			}
# 			# if we dont have one on the stack
# 			else {
# 				die "unbalanced >";
# 			}
# 		}
# 		else {
# 			%events<string>($token);
# 		}
# 	}
#     print "\n";
# 	# if we find we still have some modifiers
# 	# on the stack, then we have an error, so 
# 	# we need to throw an exception about it.
# 	(+@modifier_stack) 
# 		|| die "Unbalanced modifier(s) found (" ~ join(", ", @modifier_stack) ~ ") in the string (@tokens)";
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

=item B<parse (Str $filename, Hash %event_handlers)>

The main C<parse> function currently takes a C<$filename> and a Hash of C<%event_handlers> 
which it uses to process each line of the file.

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
