use v6;

class URI::_segment {
  # Represents a generic path_segment so that it can be treated as
  # a string too.
  use URI::Escape <uri_unescape>

  has @.segment;
  submethod BUILD (Str $segment) {
    @.segment = split ";", $segment, -1;
    @.segment[0] = uri_unescape @.segment[0]; # XXX - can this be (.=)-ified? :)
  }

  multi sub *coerce:<as> (::?CLASS $self, Str ::to) { @.segment[0] }
}

1;
