use t::lib::Emitter;

plan tests => 53;

run_tests;

__DATA__

=== TEST 1:
--- token: a
--- Layout
<global>
       <constant />
</global>

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
  my $bool;
  my @pos;
  # XXX :pos(X) takes the precedence over :continue ?
  if (defined $_[3]{p}) {
    push @pos, $_[3]{p} || 0;
  } elsif ($_[3]{continue}) {
    push @pos, (pos($_[1]) || 0) .. length($s);
  } else {
    push @pos, 0..length($s);
  }
  for my $pos ( @pos ) {
    my %index;
    my @match;
    my %named;
    $bool = 1;
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
--- Layout
<global>
       <alt>
           <group>
             <constant />
           </group>
           <group>
             <constant />
           </group>
       </alt>
</global>

--- alt
       (
           ( $pad{I1170} = $pos or 1 )
           && (

           ## <group />

           )
         || (
           ( ( $bool = 1 ) && ( $pos = $pad{I1170} ) or 1 )
           && 
           ## <group />

         )
       )
--- constant
             ( ( substr( $s, $pos, 1 ) eq 'a' )
                 ? ( $pos += 1 or 1 )
                 : 0
             )



=== TEST 3: concat
--- token: ab
--- Layout
<global>
      <concat>
         <constant />
         <constant />
      </concat>
</global>

--- concat
       (

         ## <constant />

       &&

         ## <constant />

       )



=== TEST 4: metasyntax '...'
--- token: "'a'"
--- Layout
<global>
       <constant />
</global>

--- constant
       ( ( substr( $s, $pos, 1 ) eq 'a' )
           ? ( $pos += 1 or 1 )
           : 0
       )



=== TEST 5: metasyntax <$...>
--- token: ' <$a> '
--- Layout
<global>
       <group>
                 <metasyntax />
       </group>
</global>

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
--- Layout
<global>
       <group>
         <metasyntax />
       </group>
</global>

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
--- Layout
<global>
       <group>
        <metasyntax>
          <variable />
        </metasyntax>
       </group>
</global>

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
--- Layout
<global>
       <group>
         <closure />
       </group>
</global>

--- closure
         do {
           local $::_V6_SUCCEED = 1;
           $::_V6_MATCH_ = $m;
           $m->data->{capture} = \( sub { return $_[0]->[0].sqrt }->( $m ) ); 
           $bool = $::_V6_SUCCEED;
           $::_V6_MATCH_ = $m if $bool; 
           return $m if $bool; 
         }



=== TEST 9: alt
--- token: 'a|b'
--- Layout
<global>
       <alt>
           <constant />
           <constant />
       </alt>
</global>

--- alt
       (
           ( $pad{I1178} = $pos or 1 )
           && (

           ## <constant />

           )
         || (
           ( ( $bool = 1 ) && ( $pos = $pad{I1178} ) or 1 )
           && 
           ## <constant />

         )
       )



=== TEST 10: special chars
--- token: "\\d \\s"
--- Layout
<global>
      <concat>
         <group>
           <perl5 />
         </group>
         <perl5 />
      </concat>
</global>

--- concat
       (

         ## <group />

       &&
         ## <perl5 />

       )
--- perl5
           ( ( substr( $s, $pos ) =~ m/^(\d)/ )
               ? ( $pos += length( $1 ) or 1 )
               : 0
           )



=== TEST 11: closures
--- token: " a { say 'hi' } "
--- Layout
<global>
      <concat>
         <group>
           <constant />
         </group>
         <group>
           <closure />
         </group>
      </concat>
</global>

--- closure
           do { 
             local $::_V6_SUCCEED = 1;
             $::_V6_MATCH_ = $m;
             sub { say 'hi' }->( $m );
             1;
           }



=== TEST 12: closure quantifier
--- token: " a**{2} "
--- Layout
<global>
       <quant>
           <constant />
           <constant />
       </quant>
</global>

--- quant
       (
        (   ( $pad{I1182} = $pos or 1 ) &&

           ## <constant />
 ||    ( ( $pos = $pad{I1182} ) && 0 ) ) &&         (   ( $pad{I1182} = $pos or 1 ) &&

           ## <constant />
 ||    ( ( $pos = $pad{I1182} ) && 0 ) )
       )



=== TEST 13: quantifier +
--- token: " a+ "
--- Layout
<global>
       <quant>
           <constant />
           <constant />
       </quant>
</global>

--- quant
       (
        (   ( $pad{I1183} = $pos or 1 ) &&

           ## <constant />
 ||    ( ( $pos = $pad{I1183} ) && 0 ) )
       && do { while (
        (   ( $pad{I1183} = $pos or 1 ) &&

           ## <constant />
 ||    ( ( $pos = $pad{I1183} ) && 0 ) )) {}; $bool = 1 }
       )



=== TEST 14: quantifier ?
--- token: " b? "
--- Layout
<global>
       <quant>
           <constant />
       </quant>
</global>

--- quant
       (
        (   ( $pad{I1184} = $pos or 1 ) &&

           ## <constant />
 ||    ( ( $pos = $pad{I1184} ) && 0 ) )
       || ( $bool = 1 )
       )



=== TEST 15: subrule (<foo>)
--- token: " <foo> "
--- Layout
<global>
       <group>
         <named_capture />
       </group>
</global>

--- named_capture
         do {
                my $prior = $::_V6_PRIOR_;
                my $match =
                     $grammar->foo( $s, { p => $pos, positionals => [  ], args => {}, }, undef );
                $::_V6_PRIOR_ = $prior;
                if ( $match ) { $named{'foo'} = $match;
                    $pos = $match->to;
                    1
                }
                else { 0 }
            }



=== TEST 16: subrule (<?foo>)
--- token: " <?foo> "
--- Layout
<global>
       <group>
         <metasyntax />
       </group>
</global>

--- metasyntax
         do {
              my $prior = $::_V6_PRIOR_;
              my $match =
                     $grammar->foo( $s, { p => $pos, positionals => [  ], args => {}, }, undef );
              $::_V6_PRIOR_ = $prior;
              my $bool = (!$match != 1);
              $pos = $match->to if $bool;
              $match;
         }



=== TEST 17: subrule (quanlified)
--- token: " <Bar.foo> "
--- Layout
<global>
       <group>
         <named_capture />
       </group>
</global>

--- named_capture
         do {
                my $prior = $::_V6_PRIOR_;
                my $match =
                     Bar->foo( $s, { p => $pos, positionals => [  ], args => {}, }, undef );
                $::_V6_PRIOR_ = $prior;
                if ( $match ) { $named{'Bar.foo'} = $match;
                    $pos = $match->to;
                    1
                }
                else { 0 }
            }



=== TEST 18: capure (...)
--- token: " (a) "
--- Layout
<global>
       <group>
         <capture>
                 <constant />
         </capture>
       </group>
</global>

--- capture
         do{
             my $hash = do {
               my $bool = 1;
               my $from = $pos;
               my @match;
               my %named;
               $bool = 0 unless

                 ## <constant />
;
               { str => \$s, from => \$from, match => \@match, named => \%named, bool => \$bool, to => \(0+$pos), capture => undef }
             };
             my $bool = ${$hash->{'bool'}};
             $match[ 0 ] = Pugs::Runtime::Match->new( $hash );
             $bool;
         }



=== TEST 19: named capure ( ... )
--- token: ' $abc := (a) '
--- Layout
<global>
       <group>
         <named_capture>
                 <capture>
                         <constant />
                 </capture>
         </named_capture>
       </group>
</global>

--- capture
                 do{
                     my $hash = do {
                       my $bool = 1;
                       my $from = $pos;
                       my @match;
                       my %named;
                       $bool = 0 unless

                         ## <constant />
;
                       { str => \$s, from => \$from, match => \@match, named => \%named, bool => \$bool, to => \(0+$pos), capture => undef }
                     };
                     my $bool = ${$hash->{'bool'}};
                     $match[ 0 ] = Pugs::Runtime::Match->new( $hash );
                     $bool;
                 }
--- named_capture
         do{
                my $match = Pugs::Runtime::Match->new( do {
                    my $bool = 1;
                    my $from = $pos;
                    my @match;
                    my %named;
                    $bool = 0 unless 
                 ## <capture />
;
                    { str => \$s, from => \$from, match => \@match, named => \%named, bool => \$bool, to => \(0+$pos), capture => undef }
                } );
                if ( $match ) { $named{'abc'} = $match;
                    $pos = $match->to;
                    1
                }
                else { 0 }
            }



=== TEST 20: non-capture groups
--- token: ' [ a ] '
--- Layout
<global>
       <group>
         <group>
           <constant />
         </group>
       </group>
</global>

--- constant
           ( ( substr( $s, $pos, 1 ) eq 'a' )
               ? ( $pos += 1 or 1 )
               : 0
           )



=== TEST 20: non-capture groups
--- token: '[ a ]'
--- Layout
<global>
       <group>
         <constant />
       </group>
</global>

--- constant
         ( ( substr( $s, $pos, 1 ) eq 'a' )
             ? ( $pos += 1 or 1 )
             : 0
         )



=== TEST 21: named capture + [ ... ]
--- token: ' $a := [a] '
--- Layout
<global>
       <group>
         <named_capture>
                 <constant />
         </named_capture>
       </group>
</global>

--- named_capture
         do{
                my $from = $pos;
                my $bool = 
                 ## <constant />
;
                my $match = Pugs::Runtime::Match->new(
                    { str => \$s, from => \$from, match => [], named => {}, bool => \1, to => \(0+$pos), capture => undef }
                ); $named{'a'} = $match;
                $bool
            }



=== TEST 22:
--- token: ' .*? a '
--- Layout
<global>
      <concat>
         <quant>
            <concat>
               <negate>
                         <before>
                                  <concat>
                                     <group>
                                       <constant />
                                     </group>
                                  </concat>
                         </before>
               </negate>
               <group />
            </concat>
         </quant>
         <group>
           <constant />
         </group>
      </concat>
</global>

--- negate
               do{
                   my $pos1 = $pos;
                   do {
                     my $pos = $pos1;
                     my $from = $pos;
                     my @match;
                     my %named;
                     $bool = 
                         ## <before />
 ? 0 : 1;
                     $bool;
                   };
               }


