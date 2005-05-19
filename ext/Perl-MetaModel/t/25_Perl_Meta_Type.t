#!/usr/bin/pugs

use v6;
use Test;

plan 11;

use Perl::Meta::Type;

{
    my $type = Perl::Meta::Type.new(name => 'Str');
    is($type.name(), 'Str', '... it is a string type');
    is($type.sigil(), '$', '... it is a Scalar sigil');
    
    $type.sigil('@');
    is($type.sigil(), '@', '... it is now an Array sigil');

    $! = undef;
    dies_ok {
        $type.sigil('#');        
    }, '... this will throw an error';
    like($!, rx:perl5/^Incorrect sigil '#'/, '... and this is the error');
}

{
    my $type = Perl::Meta::Type.new(name => 'Hash');
    is($type.name(), 'Hash', '... it is a Hash type');
    is($type.sigil(), '%', '... it is a Hash sigil');
}

{
    my $type = Perl::Meta::Type.new(name => 'Array');
    is($type.name(), 'Array', '... it is an Array type');
    is($type.sigil(), '@', '... it is an Array sigil');
}

{
    my $type = Perl::Meta::Type.new(name => 'Sub');
    is($type.name(), 'Sub', '... it is an Sub type');
    is($type.sigil(), '&', '... it is an Sub sigil');
}
