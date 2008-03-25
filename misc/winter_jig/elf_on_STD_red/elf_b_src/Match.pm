
class Match {
  has $.rule;
  has $.str;
  has $.from;
  has $.to;
  has $.hash;
  method make($r,$s,$f,$t,$h) {
    my $init = { 'rule',$r,'str',$s,'from',$f,'to',$t,'hash',$h };
    self.new($init)
  };
  method match_describe() {
    my $s = $.rule~"<"~$.from~","~$.to~",'"~$.str~"',{";
    for $.hash.keys {
      my $k = $_;
      my $v = $.hash.{$k};
      my $vs = $v;
      $s = $s ~ "\n  "~$k~" => "~self.indent_except_top($vs)~",";
    }
    if $.hash.keys.elems {$s = $s ~ "\n"}
    $s = $s ~ "}>";
  };
  method indent_except_top($s) {
    "*unimplemented*"
  }
}
