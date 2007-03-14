package Pugs::Emitter::Rule::Perl5::CharClass;

# input format:
# [
#    '+alpha'
#    '-[z]'
# ]

sub emit {
    #print Dumper( $_[0] );
    #print Dumper( @{$_[0]} );
    my @c = map { "$_" } @{$_[0]};
    #print Dumper( @c );
    my $out = '(?:';
    my $last_cmd = '';
    for my $cmd ( @c ) { 
        if ( $last_cmd eq '-'
            && substr($cmd,0,1) eq '+' 
            )
        {
            $out .= '|';
        }
        elsif ( $last_cmd eq '+'
            && substr($cmd,0,1) eq '+' 
            )
        {
            $out .= '|';
        }
        $last_cmd = substr($cmd,0,1);

        $cmd =~ s/\.\./-/g;  # ranges
        
        # TODO - \o \O

        if    ( $cmd =~ /^ \+? \[ \\ c \[ (.*) \] \] /x ) {
            #$cmd = "(?:\\N{" . join( "}|\\N{", split( /\s*;\s*/, $1 ) ) . "})";
            $cmd = "[\\N{" . join( "}\\N{", split( /\s*;\s*/, $1 ) ) . "}]";
        }
        elsif ( $cmd =~ /^ \+? \[ \\ C \[ (.*) \] \] /x ) {
            #$cmd = "(?!\\N{" . join( "}|\\N{", split( /\s*;\s*/, $1 ) ) . "})\\X";
            $cmd = "[^\\N{" . join( "}\\N{", split( /\s*;\s*/, $1 ) ) . "}]";
        }
        elsif ( $cmd =~ /^ -  \[ \\ C \[ (.*) \] \] /x ) {
            #$cmd = "(?:\\N{" . join( "}|\\N{", split( /\s*;\s*/, $1 ) ) . "})";
            $cmd = "[\\N{" . join( "}\\N{", split( /\s*;\s*/, $1 ) ) . "}]";
        }
        elsif ( $cmd =~ /^ -  \[ \\ c \[ (.*) \] \] /x ) {
            #$cmd = "(?!\\N{" . join( "}|\\N{", split( /\s*;\s*/, $1 ) ) . "})\\X";
            $cmd = "[^\\N{" . join( "}\\N{", split( /\s*;\s*/, $1 ) ) . "}]";
        }

        
        elsif ( $cmd =~ /^ \+? \[ \\ x \[ (.*) \] \] /x ) {
            $cmd = "(?:\\x{$1})";
        }
        elsif ( $cmd =~ /^ \+? \[ \\ X \[ (.*) \] \] /x ) {
            $cmd = "(?!\\x{$1})\\X";
            #$cmd = "[^\\x{$1}]";
        }
        elsif ( $cmd =~ /^ -  \[ \\ X \[ (.*) \] \] /x ) {
            $cmd = "(?:\\x{$1})";
            #$cmd = "[\\x{$1}]";
        }
        elsif ( $cmd =~ /^ -  \[ \\ x \[ (.*) \] \] /x ) {
            $cmd = "(?!\\x{$1})\\X";
        }
        
        
        elsif ( $cmd =~ /^ - \s* \[ (.*) /x ) {
           $cmd = '[^' . $1;
        } 
        elsif ( $cmd =~ /^ - \s* (.*) /x ) {
           my $name = $1;
           $cmd = ( $name =~ /^is/ )
                ? "\\P{$name}"
                : "[^[:$name:]]";
        } 
        elsif ( $cmd =~ /^ \+ \s* \[ (.*) /x ) {
           $cmd = '[' . $1;
	    }
        elsif ( $cmd =~ /^ \+ \s* (.*) /x ) {
           my $name = $1;
           $cmd = ( $name =~ /^is/ )
                ? "\\p{$name}"
                : "[[:$name:]]";
        } 
        
        $out .= '(?=' . $cmd . ')';
    }
    $out .= ')\X';
    return $out;
}

1;

