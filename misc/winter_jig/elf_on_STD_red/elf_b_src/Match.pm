
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
      my $v = $.hash{$k};
      my $vs = 'undef';
      if defined($v) {
        $vs = $v.match_describe;
      }
      $s = $s ~ "\n  "~$k~" => "~self.indent_except_top($vs)~",";
    }
    if $.hash.keys.elems {$s = $s ~ "\n"}
    $s = $s ~ "}>";
  };
  method indent($s) {
    $s.re_gsub(/(?m:^(?!\Z))/,'  ')
  };
  method indent_except_top($s) {
    $s.re_gsub(/(?m:^(?<!\A)(?!\Z))/,'  ')
  };
  method match_string() {
    $.str
  };
};
class ARRAY {
  method match_describe() {
    ("[\n" ~
     Match.indent(self.map(sub($e){$e.match_describe}).join(",\n")) ~
     "\n]")
  }
};
class SCALAR {
  method match_describe() {
    "'"~self~"'"
  }
}
