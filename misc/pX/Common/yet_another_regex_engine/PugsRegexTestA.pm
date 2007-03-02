# PERL5_V6_IMPLEMENTATION=PugsRegexTestA perl -I. -Mv6-x ../../../../t/regex/from_perl6_rules/anchors.t
# PERL5_V6_IMPLEMENTATION=PugsRegexTestA prove --perl 'perl -I. -Mv6-x' ../pugsx1/t/regex/from_perl6_rules/anchors.t
package PugsRegexTestA;
use strict;
# Approach: Filter away p6-isms not rule related, then hand off rule filtering.
sub quote {
  my($s)=@_;
  "q{$s}";
}
sub number_lines {
  my($s,$start)=@_;
  my $cnt = defined($start)?$start:1;
  $s =~ s/^(.*)/my $sp = 6 - length($cnt);"\# ".$cnt++.(" " x $sp).$1/meg;
  $s;
}
use Regexp::Common;
require Regexp_ModuleA;
use Filter::Simple sub {
  if(/\#dcf1d2b7b98/){ use Carp; Carp::confess("Filter called twice."); }
  my $head = <<'END';
#line 2 "PugsRegexTestA header"
#dcf1d2b7b98
#require Regexp_ModuleA;
sub regex_api0 { "Regexp::ModuleA::Api::RegexApi0" }
our $M;
sub rx_match { my $pkg = caller; $M = regex_api0->create_and_match('regex',undef,@_,pkg=>$pkg) }
BEGIN{ Regexp::ModuleA::Api::PreludeA->import };
END
  s/\b(if\s+(?!\())([^\{]+)\{/$1($2)\{/g;
  my $re_lit = qr/\$\w+|$RE{quoted}/;
  my $re_rx = qr{m?((?:(?<=m)\:[:\w]+)?)$RE{delimited}{-delim=>'/'}{-keep}};
  s/($re_lit)\s*~~\s*$re_rx/'rx_match('.quote($5).",$1,mods=>q{$2})"/eg;
  s/([\$\@\%\w<>\/]+)\.(keys)/$2($1)/g;
  my $mark = "\014";
  s/\$\//\$M$mark/g;
  s{\$/<(\w+)>}{\$/->{$1}}g;
  while (s/$mark<(\w+)>/->{$1}$mark/g ||
	 s/$mark\[(\d+)\]/->[$1]$mark/g) {}
  s/$mark//g;
  s/:(\w+)<([^>]*?)>/$1=>q{$2}/g;
  $_ = Regexp::ModuleA::Api::FilterRegexDefinitionsA::filter_string($_);
  $_ = Regexp::ModuleA::Api::FilterWithenvA::filter_string($_);

  my $ref = number_lines($_,0);
  $_ = $head.$_;
  print STDERR number_lines($head).$ref if $ENV{DEBUG_PugsRegexTestA};
  $_;
};

1;
__END__
