use t::lib::Emitter;

plan tests => 5;

run_tests;

__DATA__

=== TEST 1:
--- token: a
--- constant
       ( ( substr( $s, $pos, 1 ) eq 'a' )
           ? ( $pos += 1 or 1 )
           : 0
       )
--- global
## sigspace: 0
## ratchet: 1
do { my $rule; $rule = sub {
  my $grammar = $_[0];
  my $s = $_[1];
  $_[3] = $_[2] unless defined $_[3]; # backwards compat
  no warnings 'substr', 'uninitialized', 'syntax';
  my %pad;
  my $m;
  for my $pos ( defined $_[3]{p} && ! $_[3]{continue}
        ? $_[3]{p}
        : ( ( $_[3]{p} || pos($_[1]) || 0 ) .. length( $s ) ) ) {
    my %index;
    my @match;
    my %named;
    my $bool = 1;
    $named{KEY} = $_[3]{KEY} if exists $_[3]{KEY};
    $m = Pugs::Runtime::Match->new( {
      str => \$s, from => \(0+$pos), to => \($pos),
      bool => \$bool, match => \@match, named => \%named, capture => undef,
    } );
    {
      my $prior = $::_V6_PRIOR_;
      local $::_V6_PRIOR_ = $prior;
      $bool = 0 unless
       ## <constant />
;
    }
    if ( $bool ) {
      my $prior = $::_V6_PRIOR_;
      $::_V6_PRIOR_ = sub {
        local $main::_V6_PRIOR_ = $prior;
        $rule->(@_);
      };
      #warn "pos2 = ", $pos, "\n";
      pos($_[1]) = $pos if $_[3]{continue};
      last;
    }
  } # /for
  $::_V6_MATCH_ = $m;
  return $m;
} }



=== TEST 2:
--- token: 'a | b'
--- alt
       (
           ( $pad{I1193} = $pos or 1 )
           && (
            (   ( $pad{I1194} = $pos or 1 ) && 
             ## <constant />
 ||    ( ( $pos = $pad{I1194} ) && 0 ) )
           )
         || (
           ( ( $bool = 1 ) && ( $pos = $pad{I1193} ) or 1 )
           &&             (   ( $pad{I1195} = $pos or 1 ) && 
             ## <constant />
 ||    ( ( $pos = $pad{I1195} ) && 0 ) )
         )
       )
--- constant
             ( ( substr( $s, $pos, 1 ) eq 'a' )
                 ? ( $pos += 1 or 1 )
                 : 0
             )



