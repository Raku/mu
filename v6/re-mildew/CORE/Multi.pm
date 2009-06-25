role Multi {
  has $.candidates;

  method postcircumfix:<( )>(\$capture, :$cc) {
    my sub ACCEPTS($candidate) {
        $candidate.signature.ACCEPTS((|$capture));
    }
    my $candidates = grep &ACCEPTS,self.candidates;

    if &infix:<==>:(int,int).($candidates.elems,1) {
        $candidates.[0].postcircumfix:<( )>((|$capture), :cc($cc.FETCH));
    } elsif &infix:<==>:(int,int).($candidates.elems,0) {
        say "signature mismatch failure";
       ::Exception.new.throw;
       #my $e = ::SignatureMismatchFailure.new();
       #$e.multi = self;
       #$e.capture = $capture;
       #$e.throw;
    } else {
        say "ambiguous dispatch";
        ::Exception.new.throw;
        #my $e = ::AmbiguousDispatchFailure.new();
        #$e.multi = self;
        #$e.capture = $capture;
        #$e.candidates = $candidates;
        #$e.throw;
    }
  }
  method BUILDALL() {
    self.candidates = ::Array.new;
  }
  method get_outer_candidates($name,$scope) {
     my $outer = $scope.outer;
     loop {
       if $outer.exists((|$name)) {
         my $i = 0;
         my $multi = $outer.lookup((|$name));
         map(sub ($candidate) {self.candidates.push((|$candidate))},$multi.candidates);
         return;
       } else {
         if $outer.outer {
           $outer = $outer.outer;
         } else {
           return;
         }
       }
     }
    }
}
