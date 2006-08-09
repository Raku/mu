
package Pugs::Runtime::Common;

use strict;
use warnings;

our %quote = (
    # left => # right
    "'"    => "'",    
    '"'    => '"',    
    '/'    => '/',
    '!'    => '!',
    '('    => ')',
    '['    => ']',
    '{'    => '}',
    '<'    => '>',
    '<<'   => '>>',
    '«'    => '»',    
    '“'    => '”',    
    '‘'    => '’',
);

our %perl6_name = (
    # perl 5        => # perl 6
    '%::ENV'        => '%*ENV',  
    '$^O'           => '$*OS',  
    '$$'            => '$*PID',  
    '$0'            => '$*EXECUTABLE_NAME',  
    '$0'            => '$*PROGRAM_NAME',
    
    '\\*STDERR'     => '$*ERR',  

    '__FILE__'      => '$?FILE',
    
    '$::_V6_ERR_'   => '$!',
    '$::_V6_MATCH_' => '$/',
    '$::_V6_STDIN'  => '$*IN',  
    '$::_V6_STDOUT' => '$*OUT',  
);
our %perl5_name = reverse %perl6_name;

sub mangle_ident {
    my $s = shift;
    Carp::confess unless defined $s;
    $s =~ s/ ([^a-zA-Z0-9_:]) / '_'.ord($1).'_' /xge;
    return $s;
}

sub mangle_var {
    my $s = $_[0];
    #warn "mangle: $s";
    return $perl5_name{$s} if exists $perl5_name{$s};
    substr($s,1) =~ s/ ([^a-zA-Z0-9_:]) / '_'.ord($1).'_' /xge;
    return $s;
}

1;
