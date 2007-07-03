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
do { my $rule; $rule = sub {
  my $grammar = $_[0];
  my $s = $_[1];
  no warnings 'substr', 'uninitialized', 'syntax';
  my %pad;
  my $m;
  for my $pos ( defined $_[3]{p} && ! $_[3]{continue}
        ? $_[3]{p}
        : ( ( $_[3]{p} || 0 ) .. length( $s ) ) ) {
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
           ( $pad{I1656} = $pos or 1 )
           && (
            (   ( $pad{I1657} = $pos or 1 ) && 
             ## <constant />
 ||    ( ( $pos = $pad{I1657} ) && 0 ) )
           )
         || (
           ( ( $bool = 1 ) && ( $pos = $pad{I1656} ) or 1 )
           &&             (   ( $pad{I1658} = $pos or 1 ) && 
             ## <constant />
 ||    ( ( $pos = $pad{I1658} ) && 0 ) )
         )
       )
--- constant
             ( ( substr( $s, $pos, 1 ) eq 'a' )
                 ? ( $pos += 1 or 1 )
                 : 0
             )



