use v6;

class URI::http isa URI::_server trusts URI {
  method default_port() { 80 }

  method canonical() {
    my $other = .SUPER::canonical; # XXX - correct?

    my $slash_path =
      defined  $other.authority &&
      !length  $other.path      &&
      !defined $other.query;

    if $slash_path {
      $other .= clone if $other =:= $self;
      $other.path = "/";
    }

    return $other;
  }
}

1;
