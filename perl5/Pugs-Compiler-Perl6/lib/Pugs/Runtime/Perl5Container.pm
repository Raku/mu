use Data::Dumper;

package Pugs::Runtime::Perl5Container::Array;
use strict;
use warnings;
use Pugs::Runtime::Perl6;

# my $a = bless [ 1, 2, 3 ], 'Pugs::Runtime::Perl5Container::Array';

use overload (
    # '@{}'    => # native
    '%{}'    => \&hash,
    'bool'   => \&elems,
    '${}'    => \&values,
    '""'     => \&str,
    '0+'     => \&elems,
    fallback => 1,
);
sub elems { scalar @{$_[0]} }
sub WHICH { 'Array' }
sub isa { 
    $_[1] eq 'Array' || Universal::isa( @_ ) 
}
sub keys  { 
    bless [ 0 .. $#{$_[0]} ], 'Pugs::Runtime::Perl5Container::Array';
}
sub values { $_[0] }
sub kv {
    bless [
        map { $_, $_[0][$_] }
            0 .. $#{$_[0]}
    ], 'Pugs::Runtime::Perl5Container::Array';
}
sub perl {
    my $dumped = join(', ', 
        map { defined $_ 
                ? $_
                : 'undef'
            } 
            @{$_[0]} );
    return '(' . $dumped . ')';
}
sub str {
    join(' ', 
        map { defined $_ 
                ? $_
                : 'undef'
            } 
            @{$_[0]} );
}
sub hash {
    return bless { @{$_[0]} }, 'Pugs::Runtime::Perl5Container::Hash' 
}
sub map {
    my ($array, $code) = @_;
    my $run = ref($code) eq 'Pugs::Runtime::Perl6::Routine' 
        ? $code->code 
        : $code;
    my $arity = Pugs::Runtime::Perl6::Routine->new($run)->arity || 1;

    if ( $arity == 1 ) {
        return bless [
                map { $run->( [ \$_ ], {} ) } @{$array}
            ], 'Pugs::Runtime::Perl5Container::Array';
    }

    my @result;
    my $i = 0;
    while ( $i <= $#{$array} ) {
       	my @x = @{$array}[$i..$i+$arity-1];
        push @result, $run->([map { \$_ } @x], {});
        $i += $arity;
    }
    return bless \@result, , 'Pugs::Runtime::Perl5Container::Array';
}
sub sort  { 
    # TODO
    bless [ sort @{$_[0]} ], 'Pugs::Runtime::Perl5Container::Array';
}
sub yaml {
    eval { use YAML::Syck };
    $YAML::Syck::ImplicitTyping = 1;
    Dump( @{$_[0]} );
}

    sub value { 
        # XXX misdispatched pair.value
        $_[0]
    }
    
  
package Pugs::Runtime::Perl5Container::Hash;
use strict;
use warnings;

# my $a = bless { 1 => 2 }, 'Pugs::Runtime::Perl5Container::Hash';

use overload (
    '@{}'    => \&array,
    # '%{}'    =>   # native
    'bool'   => \&elems,
    '${}'    => \&values,
    '""'     => \&str,
    '0+'     => \&elems,
    fallback => 1,
);
sub elems { scalar keys %{$_[0]} }
sub WHICH { 'Hash' }
sub isa { 
    $_[1] eq 'Hash' || Universal::isa( @_ ) 
}
sub keys  { 
    bless [ CORE::keys %{$_[0]} ], 'Pugs::Runtime::Perl5Container::Array';
}
sub values { 
    bless [ CORE::values %{$_[0]} ], 'Pugs::Runtime::Perl5Container::Array';
}
sub kv {
    bless [  %{$_[0]}  ], 'Pugs::Runtime::Perl5Container::Array';
}
sub perl {
    "{ " .
    join( "",
        map {
            $_ . " => " .
            ( defined $_[0]{$_}
                ? $_[0]{$_}
                : 'undef' )
            . ", "
        }
        CORE::keys %{$_[0]}
    )
    . "}";
}
sub str {
    join( "\n",
        map {
            $_ . "\t" . 
            ( defined $_[0]{$_}
                ? $_[0]{$_}
                : 'undef' )
        }
        CORE::keys %{$_[0]}
    );
}
sub array {
    bless [  %{$_[0]}  ], 'Pugs::Runtime::Perl5Container::Array';
}
sub map {
    $_[0]->array->map( $_[1] )
}
sub yaml {
    eval { use YAML::Syck };
    $YAML::Syck::ImplicitTyping = 1;
    Dump( %{$_[0]} );
}
    
    sub value { 
        # XXX misdispatched pair.value
        $_[0]
    }
    
package Pugs::Runtime::Perl5Container::Pair;
use strict;
use warnings;
use base 'Pugs::Runtime::Perl5Container::Hash';

# my $a = bless { 1 => 2 }, 'Pugs::Runtime::Perl5Container::Pair';

use overload (
    '@{}'    => \&array,
    # '%{}'    =>   # native
    'bool'   => \&elems,
    '${}'    => \&values,
    '""'     => \&str,
    '0+'     => \&elems,
    fallback => 1,
);

sub key { 
    ( CORE::keys %{$_[0]} )[0]
}
sub value { 
    ( CORE::values %{$_[0]} )[0]
}
    
package Pugs::Runtime::Perl5Container::Code;
use strict;
use warnings;

# my $a = bless sub { 1 }, 'Pugs::Runtime::Perl5Container::Code';

use overload (
    # '&{}'    => native
    '@{}'    => \&array,
    '%{}'    => \&hash,
    'bool'   => \&elems,
    '${}'    => \&values,
    '""'     => \&str,
    '0+'     => \&elems,
    fallback => 1,
);
sub elems { 
    # TODO
    scalar $_[0]->() 
}
sub WHICH { 'Code' }
sub isa { 
    $_[1] eq 'Code' || Universal::isa( @_ ) 
}
sub keys  { 
    # TODO
    bless [ CORE::keys %{$_[0]} ], 'Pugs::Runtime::Perl5Container::Array';
}
sub values { 
    # TODO
    bless [ CORE::values %{$_[0]} ], 'Pugs::Runtime::Perl5Container::Array';
}
sub kv {
    # TODO
    bless [  %{$_[0]}  ], 'Pugs::Runtime::Perl5Container::Array';
}
sub perl {
    # TODO
    "sub {...}"
}
sub str {
    # TODO
    "sub {...}"
}
sub array {
    # TODO
    bless [  %{$_[0]}  ], 'Pugs::Runtime::Perl5Container::Array';
}
sub map {
    # TODO
    $_[0]->array->map( $_[1] )
}
sub yaml {
    # TODO
    eval { use YAML::Syck };
    $YAML::Syck::ImplicitTyping = 1;
    Dump( $_[0] );
}
    
1;

