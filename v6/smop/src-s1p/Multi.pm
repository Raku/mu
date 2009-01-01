use ClassHOW;
class Multi {
  has $.name;
  has @.variants;

  method postcircumfix:<( )>(\$capture, :$cc) {
    unless $cc {
        $cc = &?ROUTINE.caller();
    }
    my @all_variants;
    my sub traverse_scopes($scope) {
      if $scope.exists(self.name) {
	@all_variants.push($scope{self.name}.variants);
      }
      if $scope.outer {
	traverse_scopes($scope.outer);
      }
    }
    traverse_scopes(&?ROUTINE.lexical);
    my @candidates;
    my $iterator = @all_variants.Iterator();
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
