#!/usr/bin/pugs

use v6;
require Test;

=pod

Test handling of -I.

Multiple C<-I> switches are supposed to
prepend left-to-right:

  -Ifoo -Ibar
  
should make C<@INC> look like:

  foo
  bar
  ...

=cut

my $fragment = '-e "@INC.perl.say"';

# ugly syntax due to Perl6 list/comma parsing bug
my @tests = (
    'foo'
  , 'foo$bar'
  , 'foo bar$baz'
  # What about ,'foo$foo' ?
);

plan +@tests;

diag "Running under $?OS";

my ($pugs,$redir) = ("./pugs", ">");

if ($?OS eq "MSWin32") {
  $pugs = 'pugs.exe';
  $redir = '>';
};

sub run_pugs ($c) {
  my $tempfile = "temp-ex-output";
  my $command = "$pugs $c $redir $tempfile";
  diag $command;
  system $command;
  my $res = slurp $tempfile;
  unlink $tempfile;
  return $res;
}

for @tests -> $t {
  diag $t;
  my @dirs = split('$',$t);
  my $command;
  # This should be smarter about quoting
  # (currently, this should work for WinNT and Unix shells)
  $command = join " ", map { qq("-I$_") }, @dirs;
  my $got = run_pugs( $command ~ " $fragment" );
  
  my @got = eval $got;
  @got = @got[ 0..@dirs-1 ];
  # diag @got;
  my @expected = @dirs;
  # diag @expected;
  
  is @got, @expected, "'" ~ @dirs ~ "' works";
}
