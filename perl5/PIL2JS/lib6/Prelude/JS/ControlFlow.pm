sub JS::Root::return(*@args) is primitive {
  PIL2JS::Internals::generic_return(5)(@args);  # XXX hardcoded
}

sub JS::Root::leave(*@args) is primitive {
  PIL2JS::Internals::generic_return(3)(@args);  # XXX hardcoded
}

sub statement_control:<loop>($pre, Code $cond, Code $body, Code $post) is primitive {
  JS::inline('
    function (pre, cond, body, post) {
      try {
        for(pre; cond(); post()) {
          try {
            body();
          } catch(err) {
            if(err instanceof PIL2JS.ControlException.next) {
              // Ok;
            } else {
              throw err;
            }
          }
        }
      } catch(err) {
        if(err instanceof PIL2JS.ControlException.last) {
          return undefined;
        } else {
          throw err;
        }
      }
      return undefined;
    }
  ').($pre, $cond, $body, $post);
}

sub JS::Root::last() is primitive { JS::inline "throw(new PIL2JS.ControlException.last())"; 1 }
sub JS::Root::next() is primitive { JS::inline "throw(new PIL2JS.ControlException.next())"; 1 }

sub statement_control:<while>(Code $cond, Code $body) is primitive {
  JS::inline('
    function (cond, body) {
      var ret = undefined;
      while(ret = cond()) {
        body();
      }
      return ret;
    }
  ').($cond, $body);
}

sub statement_control:<until>(Code $cond, Code $body) is primitive {
  JS::inline('
    function (cond, body) {
      var ret = undefined;
      while(!(ret = cond())) {
        body();
      }
      return ret;
    }
  ').($cond, $body);
}

sub statement_control:<if>(Bool $cond, Code $true, Code $false) is primitive {
  JS::inline('
    function (cond, t, f) {
      return cond ? t() : f();
    }
  ').($cond, $true, $false);
}

sub statement_control:<unless>(Bool $cond, Code $true, Code $false) is primitive {
  statement_control:<if>(!$cond, $true, $false);
}

sub statement_control:<for>(@array is copy, Code $code) is primitive {
  my $arity = $code.arity;
  die "Can't use 0-ary subroutine as \"for\" body!" if $arity == 0;

  while(+@array > 0) {
    my @args = ();
    my $i; loop $i = 0; $i < $arity; $i++ {
      push @args: @array.shift;
    }
    $code(*@args);
  }
  undef;
}


sub JS::Root::try(Code $code) is primitive {
  JS::inline('new PIL2JS.Box.Constant(function (args) {
    var cxt = args[0], code = args[1];
    var ret = new PIL2JS.Box.Constant(undefined);
    try { ret = code.GET()([PIL2JS.Context.ItemAny]) } catch(err) {
      if(err instanceof PIL2JS.ControlException.ret) {
        throw err;
      } else {
        _24main_3a_3a_21.STORE(new PIL2JS.Box.Constant(err.toString()));
        return new PIL2JS.Box.Constant(undefined);
      }
    }
    return ret;
  })')($code);
}

sub JS::Root::warn(Str *@msg) is primitive { $JS::PIL2JS.warn(@msg.join("")) }
sub JS::Root::die(Str *@msg)  is primitive { $JS::PIL2JS.die(@msg.join(""))  }

