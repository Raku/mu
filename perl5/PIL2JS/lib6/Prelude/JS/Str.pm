sub JS::Root::substr(Str $str, Int $a, Int $b) is primitive {
  JS::inline('function (str, a, b) {
    return String(str).substr(Number(a), Number(b));
  }')($str, $a, $b);
}

method split(Str $self: Str $splitter) { split $splitter, $self }
sub JS::Root::split(Str $splitter, Str $str) is primitive {
  JS::inline('
    function (splitter, str) {
      return String(str).split(String(splitter));
    }
  ')($splitter, $str);
}

method uc(Str $self:) { JS::inline('function (str) { return str.toUpperCase() }')($self) }
method lc(Str $self:) { JS::inline('function (str) { return str.toLowerCase() }')($self) }
