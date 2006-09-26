use Data::Dumper;

package Pugs::Runtime::Perl5Container::Array;
use strict;
use warnings;

# my $a = bless [ 1, 2, 3 ], 'Pugs::Runtime::Perl5Container::Array';

use overload (
    # '@{}'    => # native
    '%{}'    => \&hash,
    'bool'   => \&elems,
    '${}'    => \&values,
    '""'     => \&perl,
    '0+'     => \&elems,
    fallback => 1,
);
sub elems { scalar @{$_[0]} }
sub WHICH { 'Array' }
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
sub hash {
    return bless { @{$_[0]} }, 'Pugs::Runtime::Perl5Container::Hash' 
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
    
1;

