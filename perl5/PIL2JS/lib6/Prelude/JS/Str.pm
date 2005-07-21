sub JS::Root::substr(Str $str, Int $a, Int $b) is primitive {
  JS::inline('function (str, a, b) { return str.substr(a, b) }')($str, $a, $b);
}


