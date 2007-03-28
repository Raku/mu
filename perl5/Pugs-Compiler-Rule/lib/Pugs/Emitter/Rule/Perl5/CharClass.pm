package Pugs::Emitter::Rule::Perl5::CharClass;

use strict;
use Data::Dumper;

use vars qw( %char_class );
BEGIN {
    %char_class = map { $_ => 1 } qw( 
        alpha alnum ascii blank
        cntrl digit graph lower
        print punct space upper
        word  xdigit
    );
}

# input format:
# [
#    '+alpha'
#    '-[z]'
# ]

# TODO - set composition logic
# ( ( ( before +alpha ) | before +digit ) before not-alpha ) before not-digit )

sub emit {
    #print Dumper( $_[0] );
    #print Dumper( @{$_[0]} );
    my @c = map { "$_" } @{$_[0]};
    #print Dumper( @c );
    my $out = '';
    #my $last_cmd = '';
    for ( @c ) { 
        my ( $op, $cmd ) = /(.)(.*)/;
        
        $cmd =~ s/\s//g;

        #if ( $last_cmd eq '-'
        #    && substr($cmd,0,1) eq '+' 
        #    )
        #{
        #    $out .= '|';
        #}
        #$last_cmd = substr($cmd,0,1);

        $cmd =~ s/\.\./-/g;  # ranges
        
        # TODO - \o \O

        if    ( $cmd =~ /^ \[ \\ c \[ (.*) \] \] /x ) {
            #$cmd = "(?:\\N{" . join( "}|\\N{", split( /\s*;\s*/, $1 ) ) . "})";
            $cmd = "[\\N{" . join( "}\\N{", split( /\s*;\s*/, $1 ) ) . "}]";
        }
        elsif ( $cmd =~ /^ \[ \\ C \[ (.*) \] \] /x ) {
            #$cmd = "(?!\\N{" . join( "}|\\N{", split( /\s*;\s*/, $1 ) ) . "})\\X";
            $cmd = "[^\\N{" . join( "}\\N{", split( /\s*;\s*/, $1 ) ) . "}]";
        }

        
        elsif ( $cmd =~ /^ \[ \\ x \[ (.*) \] \] /x ) {
            $cmd = "(?:\\x{$1})";
        }
        elsif ( $cmd =~ /^ \[ \\ X \[ (.*) \] \] /x ) {
            $cmd = "(?!\\x{$1})\\X";
            #$cmd = "[^\\x{$1}]";
        }
        
        
        elsif ( $cmd =~ /^ \s* \[ (.*) /x ) {
           $cmd = '[' . $1;
	    }
        elsif ( $cmd =~ /^ \s* (.*) /x ) {
           my $name = $1;
           $cmd = ( exists $char_class{$name} )
                ? "[[:$name:]]"
                : "\\p{$name}";
        } 
        
        if ( $op eq '+' ) {
            $out .= 
                ( $out eq '' )
                ? '(?=' . $cmd . ')'
                : '|(?=' . $cmd . ')';
        }
        elsif ( $op eq '-' ) {
            $out .= '(?!' . $cmd . ')';
        }
        else {
            #print Dumper( @c ), ' == ', $out, "\n";
            die "invalid character set op: $op";
        }
    }
    $out = "(?:$out)\\X";

    #print Dumper( @c ), ' == ', $out, "\n";

    return $out;
}

1;

