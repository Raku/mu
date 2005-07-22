sub JS::Root::substr(Str $str, Int $a, Int $b) is primitive {
  JS::inline('function (str, a, b) { return str.substr(a, b) }')($str, $a, $b);
}

method split(Str $self: Str $splitter) { split $splitter, $self }
sub JS::Root::split(Str $splitter, Str $str) is primitive {
  JS::inline('
    function (splitter, str) {
      return str.split(splitter);
    }
  ')($splitter, $str);
}
