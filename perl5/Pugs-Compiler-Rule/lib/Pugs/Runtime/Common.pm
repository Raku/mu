
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

my @names = (
    # perl 5        => # perl 6
    '%::ENV'        => '%*ENV',  
    '$^O'           => '$*OS',  
    '$$'            => '$*PID',  
    '$0'            => '$*EXECUTABLE_NAME',  
    '$0'            => '$*PROGRAM_NAME',
    '@INC'          => '@*INC',
    
    '\\*STDERR'     => '$*ERR',  

    '__FILE__'      => '$?FILE',
    
    '$::_V6_ERR_'   => '$!',
    '$::_V6_MATCH_' => '$/',
    '$::_V6_STDIN'  => '$*IN',  
    '$::_V6_STDOUT' => '$*OUT',  
    '$::_V6_BACKEND'=> '$?PUGS_BACKEND',
    '$::_V6_COMPILER_OS'      => '$?OS',
    '$::_V6_COMPILER_NAME'    => '$?COMPILER', 
    '$::_V6_COMPILER_VERSION' => '$?VERSION',
);
our %perl6_name = @names;
our %perl5_name = reverse @names;

sub mangle_ident {
    my $s = shift;
    Carp::confess unless defined $s;
    $s =~ s/ ([^a-zA-Z0-9_:] | (?<!:):(?!:) ) / '_'.ord($1).'_' /xge;
    return $s;
}

sub mangle_var {
    my $s = $_[0];
    return $perl5_name{$s} if exists $perl5_name{$s};
    substr($s,1) =~ s/ ([^a-zA-Z0-9_:] | (?<!:):(?!:)) / '_'.ord($1).'_' /xge;
    return $s;
}

1;
