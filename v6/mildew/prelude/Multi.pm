knowhow Multi {
  has $.name;
  has @.variants;

  method new() {
      my $new = ::p6opaque.^!CREATE();
      $new.^!how = ::PrototypeHow;
      $new.^!instanceof = ::Multi;
      $new.^!instance_storage.{'Multi'} = ::Hash.new;
      $new;
  }
  method postcircumfix:<( )>(\$capture, :$cc_) {
    my $cc;
    if ($cc_) {
        $cc = $cc_;
    } else {
        $cc = &?ROUTINE.back();
    }
    my $all_variants = ::Array.new;
    my sub traverse_scopes($scope) {
      if $scope.exists($.name) {
	$all_variants.push($scope.{$.name}.variants);
      }
      if $scope.outer {
        traverse_scopes($scope.outer);
      }
    }
    if &?ROUTINE.back.lexical {
        traverse_scopes(&?ROUTINE.back.lexical);
    }
    my $candidates = ::Array.new;
    my $iterator = $all_variants.Iterator();
    loop {
      my $candidate = =$iterator;
      if $candidate.signature.ACCEPTS((|$capture)) {
	$candidates.push($candidate);
      }
      CONTROL {
	if $_.^does(ControlExceptionSignatureMatched) {
          $candidate.postcircumfix:<( )>((|$capture), :cc($cc));
	} else {
	  $_.throw;
	}
      }
      CATCH {
	if $_.^does(OutOfItemsException) {
	  if $candidates {
            my $candidate = $candidates.shift;
	    if $candidates {
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
$LexicalPrelude.{'Multi'} = ::Multi;
