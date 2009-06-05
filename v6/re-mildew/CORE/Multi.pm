role Multi {
  has $.variants;

  method postcircumfix:<( )>(\$capture, :$cc) {
    my sub ACCEPTS($candidate) {
        $candidate.signature.ACCEPTS((|$capture));
    }
    my $candidates = grep &ACCEPTS,self.variants;

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
    say "BUILDALL";
    self.variants = ::Array.new;
  }
}
$LexicalPrelude.{'Multi'} := ::Multi;
