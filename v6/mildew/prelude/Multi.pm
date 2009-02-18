role Multi {
  has @.variants;

  method postcircumfix:<( )>(\$capture, :$cc) {
    my $all_variants = @.variants.clone;
    my $candidates = ::Array.new;
    loop {
      if &infix:<==>:(int,int)($all_variants.elems,0) {
        if &infix:<==>:(int,int)($candidates.elems,1) {
          return $candidates[0].postcircumfix:<( )>((|$capture), :cc($cc));
        } elsif &infix:<==>:(int,int)($candidates.elems,0) {
          my $e = ::SignatureMismatchFailure.new();
          $e.multi = self;
          $e.capture = $capture;
          $e.throw;
        } else {
          my $e = ::AmbiguousDispatchFailure.new();
          $e.multi = self;
          $e.capture = $capture;
          $e.candidates = $candidates;
          $e.throw;
        }
      } else {
        my $candidate = $all_variants.shift;
        if $candidate.signature.ACCEPTS((|$capture)) {
          $candidates.push($candidate);
        }
      }
    }
  }

}
