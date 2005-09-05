use v6;
module Kwid::Event::Parser-0.0.1;

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
    my $in_list    = 0;

    my $fh = open($filename);    
    loop {
        my $line = $fh.readline;
        last unless $line.defined; # exit as soon as possible
        $line .= chomp;
        if ($line ~~ rx:perl5{^=kwid}) {
            $is_parsing = 1;
            %events<start_document>();
        }
        else {
            if ($is_parsing) {
                given $line {
                    when rx:perl5{^=cut} { 
                        if ($in_list) {
                            %events<end_element>('list');
                            $in_list = 0;
                        }                    
                        $is_parsing = 0;
                        %events<end_document>();                    
                    }
                    when rx:perl5{^(=+)\s(.*?)$} {
                        if ($in_list) {
                            %events<end_element>('list');
                            $in_list = 0;
                        }
                        my $size = $0.bytes;
                        %events<start_element>('header', $size);
                        interpolate($1, %events);
                        %events<end_element>('header', $size);                       
                    }
                    when rx:perl5{^([\*\-]+)\s(.*?)$} {
                        if (!$in_list) {
                            %events<start_element>('list');
                            $in_list = 1;
                        }
                        my $depth = $0.bytes;
                        %events<start_element>('item');
                        interpolate($1, %events);
                        %events<end_element>('item');                     
                    }                   
                    when rx:perl5{^\s(.*?)$} {
                        if ($in_list) {
                            %events<end_element>('list');
                            $in_list = 0;
                        }                    
                        my $verbatim = "$0\n";
                        my $_line = $fh.readline;
                        while (defined($_line)             && 
                              !($_line ~~ rx:perl5{^\n$})  && 
                                $_line ~~ rx:perl5{^\s(.*?)$} ) {                                
                            $verbatim ~= "$0\n";
                            $_line = $fh.readline;
                        }           
                        $verbatim .= chomp;
                        %events<start_element>('verbatim');
                        %events<verbatim>($verbatim);
                        %events<end_element>('verbatim');                        
                    }
                    default {
                        if ($line ne '') {
                            if ($in_list) {
                                %events<end_element>('list');
                                $in_list = 0;
                            }                        
                            %events<start_element>('paragraph');
                            interpolate($line, %events);   
                            my $_line = $fh.readline;
                            while (defined($_line)             && 
                                   !($_line ~~ rx:perl5{^\n$}) ) {
                                $_line .= chomp;
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
    my @tokens = split(rx:perl5{([\*\/\`])}, $text); #`
    @tokens = @tokens.grep:{ defined($_) }; 
    # this is a memory stack for modifiers
    # it helps us track down problems
    my @modifier_stack;
    my $in_code = 0;
    for (@tokens) -> $token {    
        given $token {
            when rx:perl5{([\*\/\`])} { #`
                if ($in_code && $token ne '`') {
                    %events<string>($token);                    
                }
                else {
                    # if the stack is empty then ...
                    unless (@modifier_stack) {
                        # push the latest modifier on to it ...
                        @modifier_stack.push($token);
                        # and start an event ..
                        %events<start_modifier>($token);    
                        $in_code = 1 if $token eq '`';
                    }
                    else {
                        # if the top of the stack is 
                        # equal to our token then ...
                        if ($token eq @modifier_stack[-1]) {
                            # pop the top off ...
                            @modifier_stack.pop();
                            # and end the event ...
                            %events<end_modifier>($token);    
                            $in_code = 0 if $token eq '`';                            
                        }
                        else {
                            # push the latest modifier on to it ...
                            @modifier_stack.push($token);
                            # and start an event ..
                            %events<start_modifier>($token);                         
                            $in_code = 1 if $token eq '`';                            
                        }
                    }
                }
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

=kwid

= THOUGHTS

Some basic notes here as I develop this.

- how should I handle blocks of text which seem to still be inside a list?

Basically the item body, much like this text is.

- how should I handle *, / and ` inside text? 

One idea is that if I get to the end of the line and find no closing match, I should
ignore the opening one. However, this means I should defer events until I get to the
end of the line. I could probably do that, but it could get ugly.s

= TODO

- add in URL handling with []

=cut
