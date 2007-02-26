package SixishForRegexTests;
BEGIN{@ISA=qw(PerlMix);}
use PerlMix;

regex perlmix_compile_input {
  <pugs_t_regex_file>
}

regex pugs_t_regex_file {
  ^
  <header_cruft> \s*
  <plan> \s*
  <test_guard> \s*
  <a_string_def> \s*
  <test>+
  \} \s*
  $
}
regex header_cruft {
  [ <'use v6-alpha;'>
  | <'use Test;'>
  | <pod>
  | \s+ ::
  ]+ :::
}
regex pod {
  =pod .*? =cut
}
regex plan {
  plan \s+ \d+;
}
regex test_guard {
  if \!eval .*? else \{
}
regex a_string_def {
  my \s+ \$str .*? ]};
}
regex test {
  (ok\() \s* <expr> \s* ,\s*<msg>\s*(\);)\s+
}
regex msg {
  \' <[^\']>* \'
}
regex expr {
  [ <match>
  | <not_match>
  | <eval_match>
  ]
}
regex match {
  (\$str) \s+ ~~ \s+ <rx>
}
regex not_match {
  \!\( \s* <expr> \s* \)
}
regex eval_match {
  eval \( \' \s* <expr> \s* \'\)
}
regex an_rx {
  m \/ <[^\/]>* \/
}
sub compile__pugs_t_regex_file__ {
  my($cls,$m)=@_;
  ($m->{plan}[0]
   .$m->{a_string_def}[0]
   .join("",map{$cls->compile($_)}@{$m->{test}})
   );
}
sub compile__test__ {
  my($cls,$m)=@_;
  ($m->[0].($cls->compile($m->{expr}[0])).
   ',',($cls->compile($m->{msg}[0])).$m->[1])
}
sub compile__msg__ {
  my($cls,$m)=@_;
  "$m";
}
sub compile__match__ {
  my($cls,$m)=@_;
  my $rx = $cls->compile($m->{rx}[0]);
  my $s = $cls->quote_for_singlequotes($m->[0]);
  $rx."->match('$s')";
}
sub compile__not_match__ {
  my($cls,$m)=@_;
  "!(".$cls->compile($m->{expr}[0]).")"
}
sub compile__eval_match__ {
  my($cls,$m)=@_;
  $cls->compile($m->{expr}[0])
}
sub compile__an_rx__ {
  my($cls,$m)=@_;
  my $pat = "$m";
  $pat =~ s/^m\///;
  $pat =~ s/\/$//;
  $pat = $cls->quote_for_singlequotes($pat);
  "regex_api0->define_anon_regex('$pat')"
}


sub ok {
  my($bool,$msg)=@_;
  print "",($bool?'ok':'not ok'),"  # ",$msg,"\n";
}
sub plan {
  my($n)=@_;
  print "1..$n";
}

