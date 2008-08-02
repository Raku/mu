use v6;

use Test;

plan 7;

=begin pod

Really really really minimal s:Perl5//// tests.

Please add more!!

=end pod

unless "a" ~~ rx:P5/a/ {
  skip_rest "skipped tests - P5 regex support appears to be missing";
  exit;
}

my $foo = "foo";
$foo ~~ s:Perl5{f}=q{b};
is($foo, "boo", 'substitute regexp works');
unless $foo eq "boo" {
  skip_rest "Skipping test which depend on a previous failed test";
}

my $bar = "barrrr";
$bar ~~ s:Perl5:g{r+}=q{z};
is($bar, "baz", 'substitute regexp works with :g modifier');

my $path = "/path//to///a//////file";
$path ~~ s:Perl5:g{/+} = '/';
is($path, "/path/to/a/file", 'substitute regexp works with :g modifier');

my $baz = "baz";
$baz ~~ s:Perl5{.(a)(.)}=qq{$1$0p};
is($baz, "zap", 'substitute regexp with capturing variables works');

my $bazz = "bazz";
$bazz ~~ s:Perl5:g{(.)}=qq{x$0};
is($bazz, "xbxaxzxz", 'substitute regexp with capturing variables works with :g');

my $bad = "1   ";
$bad ~~ s:Perl5:g/\s*//;
is($bad, "1", 'Zero width replace works with :g');

{
	my $r;
	temp $_ = 'heaao';
	s:Perl5 /aa/ll/ && ($r = $_);
	is $r, 'hello', 's/// in boolean context properly defaults to $_', :todo<bug>;
}

