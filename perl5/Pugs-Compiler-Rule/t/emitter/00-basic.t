use t::lib::Emitter;

plan tests => 12;

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
           ( $pad{I1170} = $pos or 1 )
           && (
            (   ( $pad{I1171} = $pos or 1 ) &&
             ## <constant />
 ||    ( ( $pos = $pad{I1171} ) && 0 ) )
           )
         || (
           ( ( $bool = 1 ) && ( $pos = $pad{I1170} ) or 1 )
           &&             (   ( $pad{I1172} = $pos or 1 ) &&
             ## <constant />
 ||    ( ( $pos = $pad{I1172} ) && 0 ) )
         )
       )
--- constant
             ( ( substr( $s, $pos, 1 ) eq 'a' )
                 ? ( $pos += 1 or 1 )
                 : 0
             )



=== TEST 3: concat
--- token: ab
--- concat
       (
         ## <constant />

       &&
         ## <constant />

       )



=== TEST 4: metasyntax '...'
--- token: "'a'"
--- constant
       ( ( substr( $s, $pos, 1 ) eq 'a' )
           ? ( $pos += 1 or 1 )
           : 0
       )



=== TEST 5: metasyntax <$...>
--- token: ' <$a> '
--- metasyntax
                 do {
                   my $r = Pugs::Runtime::Regex::get_variable( '$a' );
                   push @match,
                     $r->match( $s, $grammar, {p => $pos}, undef );
                   $pos = $match[-1]->to;
                   !$match[-1] != 1;
                 }



=== TEST 6: metasyntax <@...>
--- token: ' <@::foo> '
--- metasyntax
         do {
                my $match;
                for my $subrule ( @::foo ) {
                    $match = $subrule->match( $s, $grammar, { p => ( $pos ), positionals => [ ], args => {} }, undef );
                    last if $match;
                }
                if ( $match ) { $named{'::foo'} = $match;
                    $pos = $match->to;
                    1
                }
                else { 0 }
            }



=== TEST 7: metasyntax <%...>
--- token: ' <%hi> '
--- metasyntax
         do{
                my $match = 
          ## <variable />
;
                if ( $match ) { $named{'hi'} = $match;
                    $pos = $match->to;
                    1
                }
                else { 0 }
            }
--- variable
          do {
            our $I1176;
            our $I1176_sizes;
            unless ( $I1176 ) {
                my $hash = \%hi;
                my %sizes = map { length($_) => 1 } keys %$hash;
                $I1176_sizes = [ sort { $b <=> $a } keys %sizes ];
                $I1176 = $hash;
            }
            my $match = 0;
            my $key;
            for ( @$I1176_sizes ) {
                $key = ( $pos <= length( $s )
                            ? substr( $s, $pos, $_ )
                            : '' );
                if ( exists $I1176->{$key} ) {
                    #$named{KEY} = $key;
                    #$::_V6_MATCH_ = $m;
                    #print "m: ", Dumper( $::_V6_MATCH_->data )
                    #    if ( $key eq 'until' );
                    $match = Pugs::Runtime::Regex::preprocess_hash( $I1176, $key )->( $s, $grammar, { p => ( $pos + $_ ), positionals => [ ], args => { KEY => $key } }, undef );
                    last if $match;
                }
            }
            if ( $match ) {
                $pos = $match->to;
                #print "match: $key at $pos = ", Dumper( $match->data );
                $bool = 1;
            }; # else { $bool = 0 }
            $match;
          }



=== TEST 8: metasyntax
--- token: ' <{ return $0.sqrt }> '
--- closure
         do { 
           local $::_V6_SUCCEED = 1;
           $::_V6_MATCH_ = $m;
           $m->data->{capture} = \( sub { return $_[0]->[0].sqrt }->( $m ) ); 
           $bool = $::_V6_SUCCEED;
           $::_V6_MATCH_ = $m if $bool; 
           return $m if $bool; 
         }



