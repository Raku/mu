use ClassHOW;
class Multi {
  has $.name;
  has @.variants;

  method postcircumfix:<( )>(\$capture, :$cc) {
    unless $cc {
        $cc = &?ROUTINE.caller();
    }
    my @variants;
    my sub traverse_scopes($scope) {
      if $scope.exists(self.name) {
	@variants.push($scope{self.name}.variants);
      }
      if $scope.outer {
	traverse_scopes($scope.outer);
      }
    }
    traverse_scopes($self.outer);
    my @candidates;
    my $iterator = @variants.Iterator();
    loop {
      my $candidate = =$iterator;
      if $candidate.signature.ACCEPTS($capture) {
	@candidates.push($candidate);
      }
      CONTROL {
	when ControlExceptionSignatureMatched {
	  $candidate.postcircumfix<( )>($capture, :cc($cc));
	}
      }
      CATCH {
	when OutOfItemsException {
	  if @candidates {
	    if @candidates.elems > 1 {
	      # this is where the disambiguator should be called!
	      fail "Ambiguous dispatch!";
	    } else {
	      @candidates[0].postcircumfix:<( )>($capture);
	    }
	  } else {
	    fail "No candidate matching capture.";
	  }
	}
      }
    }
  }
}
