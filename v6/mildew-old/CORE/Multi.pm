role Multi {
    has $.candidates;
    has $.sorted_candidates is rw;

    my sub qsort($array) {
        if &infix:<==>:(int,int)($array.elems,0) {
            ::Array.new;
        } else {
            my $partition = $array[0].signature;

            my $left  = qsort(grep sub ($elem) {&infix:<==>:(int,int)($elem.signature.compare($partition),&infix:<->:(int,int)(0,1))},$array);
            my $equal = grep(sub ($elem) {&infix:<==>:(int,int)($elem.signature.compare($partition),0)},$array);
            my $right = qsort(grep sub ($elem) {&infix:<==>:(int,int)($elem.signature.compare($partition),1)},$array);
    
            my $result = ::Array.new;
            map(sub ($x) {$result.push($x.FETCH)},$left);
            map(sub ($x) {$result.push($x.FETCH)},$equal);
            map(sub ($x) {$result.push($x.FETCH)},$right);
            $result;
        }
    }
    method postcircumfix:<( )>(\$capture, :$cc) {
        my sub ACCEPTS($candidate) {
            $candidate.signature.ACCEPTS((|$capture));
        }
        if self.sorted_candidates {
        } else {

            self.sorted_candidates = qsort(self.candidates);
        }

        my $candidates = grep &ACCEPTS,self.sorted_candidates;

        if &infix:<==>:(int,int)($candidates.elems,1) {
            $candidates[0].postcircumfix:<( )>((|$capture), :cc($cc.FETCH));
        } elsif &infix:<==>:(int,int)($candidates.elems,0) {
            say "signature mismatch failure";
           ::Exception.new.throw;
           #my $e = ::SignatureMismatchFailure.new();
           #$e.multi = self;
           #$e.capture = $capture;
           #$e.throw;
#
        } elsif &infix:<==>:(int,int)($candidates[0].signature.compare($candidates[1].signature),&infix:<->:(int,int)(0,1)) {
            $candidates[0].postcircumfix:<( )>((|$capture), :cc($cc.FETCH));
        } else {
            say "ambiguous dispatch";
            say $candidates[0].signature.compare($candidates[1].signature);
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
$LexicalPrelude{'Multi'} = ::Multi;
