#!/usr/bin/pugs

use v6;
use Test;

plan 3;
my $destroy_test = '#!/usr/bin/pugs

class Foo
{
    submethod DESTROY { say "Foo goes away" }
}

class Parent
{
    submethod DESTROY { say "Parent goes away" }
}

class Child is Parent
{
    submethod DESTROY { say "Child goes away" }
}

my $foo    = Foo.new();
my $parent = Parent.new();
my $child  = Child.new();';

my $out = open('destroy_test.p6', :w);

unless $out
{
    diag( "Could not write destroy_test.p6" );
    exit;
}

$out.say( $destroy_test );
$out.close;

my $pugs = 'pugs';
$pugs   ~= '.exe' if $*OS ~~ any<MSWin32 mingw msys cygwin>;
$pugs   ~~ s:P5<g>{/}{\\} if $*OS eq 'MSWin32';

my $res  = system( $pugs, ( map { "-I$_" } @*INC ), 'destroy_test.p6' );
if $res 
{
    my $output = slurp 'destroy.out';
    like( $output, rx:P5/Foo goes away/,
        'global destruction should collect objects...' );
    like( $output, rx:P5/Parent goes away/,
        '... of all types' );
    like( $output, rx:P5/Child goes way.+Parent goes away/,
        '... and calling all destructors' );
}
else
{
    skip( 3, "Could not launch $pugs for destroy test" );
}

END
{
    if ! %*ENV<TEST_DEBUG_FILES>
    {
        unlink 'destroy.out';
        unlink 'destroy_test.p6';
    }
}
