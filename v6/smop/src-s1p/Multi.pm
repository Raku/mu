knowhow Multi {
  has $.name;
  has @.variants;

  method postcircumfix:<( )>(\$capture, :$cc) {
    unless $cc {
        $cc = &?ROUTINE.back();
    }
    my @all_variants;
    my sub traverse_scopes($scope) {
      if $scope.exists($.name) {
	@all_variants.push($scope.{$.name}.variants);
      }
      if $scope.outer {
	traverse_scopes($scope.outer);
      }
    }
    traverse_scopes(&?ROUTINE.back.lexical);
    my @candidates;
    my $iterator = @all_variants.Iterator();
    loop {
      my $candidate = =$iterator;
      if $candidate.signature.ACCEPTS((|$capture)) {
	@candidates.push($candidate);
      }
      CONTROL {
	if ($_.^does(ControlExceptionSignatureMatched)) {
          $candidate.postcircumfix<( )>((|$capture), :cc($cc));
	} else {
	  $_.throw;
	}
      }
      CATCH {
	if ($_.^does(OutOfItemsException)) {
	  if @candidates {
            my $candidate = @candidates.shift;
	    if @candidates {
	      # this is where the disambiguator should be called!
	      fail "Ambiguous dispatch!";
	    } else {
              $candidate.postcircumfix:<( )>((|$capture), :cc($cc));
	    }
	  } else {
	    fail "No candidate matching capture.";
	  }
	} else {
	  $_.throw;
	}
      }
    }
  }
}
