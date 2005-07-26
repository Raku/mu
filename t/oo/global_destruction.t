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

my ($pugs,$redir) = ("./pugs", ">");

if($*OS eq any<MSWin32 mingw msys cygwin>) {
  $pugs = 'pugs.exe';
  $redir = '>';
};

sub nonce () { return (".$*PID." ~ int rand 1000) }

sub run_pugs ($c) {
  my $tempfile = "temp-ex-output" ~ nonce;
  my $command = "$pugs $c $redir $tempfile";
  diag $command;
  system $command;
  my $res = slurp $tempfile;
  unlink $tempfile;
  return $res;
}

my $output  = run_pugs("destroy_test.p6");

like( $output, rx:P5/Foo goes away/,
    'global destruction should collect objects...' );
like( $output, rx:P5/Parent goes away/,
    '... of all types' );
like( $output, rx:P5/Child goes away\s*Parent goes away/,
    '... and calling all destructors' );

END
{
    if ! %*ENV<TEST_DEBUG_FILES>
    {
        unlink 'destroy_test.p6';
    }
}
