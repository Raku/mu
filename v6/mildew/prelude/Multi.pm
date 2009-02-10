knowhow Multi {
  has $.name;
  has @.variants;
  my sub say($arg) {
      $OUT.print($arg,"\n"); 
  }

  method new() {
      my $new = ::p6opaque.^!CREATE();
      $new.^!how = ::PrototypeHow;
      $new.^!instanceof = ::Multi;
      $new.^!instance_storage.{'Multi'} = ::Hash.new;
      $new;
  }
  method postcircumfix:<( )>(\$capture, :$cc) {
    my $all_variants = ::Array.new;
    $all_variants.push(self.variants);
    my $candidates = ::Array.new;
    loop {
      if ($all_variants.elems.infix:<==>(0)) {
        if $candidates.elems {
          my $candidate = $candidates.shift;
          if $candidates {
            # this is where the disambiguator should be called!
            $OUT.print("Ambiguous dispatch!\n");
            my $e = ::ControlExceptionReturn.new();
            $e.routine = &?ROUTINE;
            $e.throw();
            #fail "Ambiguous dispatch!";
          } else {
            $candidate.postcircumfix:<( )>((|$capture), :cc($cc));
          }
        } else {
          $OUT.print("No candidate matching capture.\n");
          my $e = ::ControlExceptionReturn.new();
          $e.routine = &?ROUTINE;
          $e.throw();
          #fail "No candidate matching capture.";
        }
      } else {
        say "considering variant\n";
        my $candidate = $all_variants.shift;
        if $candidate.signature.ACCEPTS((|$capture)) {
          $candidates.push($candidate);
        }
      }
    }
  }
}
$LexicalPrelude.{'Multi'} = ::Multi;
