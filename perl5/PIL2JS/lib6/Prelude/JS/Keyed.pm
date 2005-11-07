# No MMD yet.
method exists (Hash|Pair|Array $self: $idx) {
  if $self.isa("Hash") {
    JS::inline('new PIL2JS.Box.Constant(function (args) {
      var hash = args[1].FETCH(), key = args[2], cc = args.pop();
      cc(new PIL2JS.Box.Constant(hash.exists(key)));
    })')(%$self, $idx);
  } elsif $self.isa("Pair") {
    $self.key eq $idx;
  } elsif $self.isa("Array") {
    JS::inline('new PIL2JS.Box.Constant(function (args) {
      var array = args[1].FETCH(), idx = Number(args[2].toNative()), cc = args.pop();
      cc(new PIL2JS.Box.Constant(
        array[idx >= 0 ? idx : array.length + idx] != undefined
      ));
    })')(@$self, $idx);
  } else {
    die ".exists does not work on objects of type {$self.ref}!";
  }
}

method delete (Hash|Array $self: *@idx) {
  if $self.isa("Hash") {
    JS::inline('new PIL2JS.Box.Constant(function (args) {
      var hash = args[1].FETCH(), keys = args[2].FETCH(), cc = args.pop();
      var ret  = [];
      for(var i = 0; i < keys.length; i++) {
        var deleted = hash.delete_key(keys[i]);
        ret.push(deleted == undefined ? new PIL2JS.Box.Constant(undefined) : deleted);
      }
      cc(new PIL2JS.Box.Constant(ret));
    })')(%$self, @idx);
  } elsif $self.isa("Array") {
    JS::inline('new PIL2JS.Box.Constant(function (args) {
      var array = args[1].FETCH(), idxs = args[2].toNative(), cc = args.pop();
      var ret   = [];
      for(var i = 0; i < idxs.length; i++) {
        var idx = Number(idxs[i]) >= 0 ? Number(idxs[i]) : array.length + Number(idxs[i]);
        ret.push(array[idx] == undefined ? new PIL2JS.Box.Constant(undefined) : array[idx]);
        delete array[idx];
        if(idx == array.length - 1) array.length--;
      }
      cc(new PIL2JS.Box.Constant(ret));
    })')(@$self, @idx);
  } else {
    die ".delete does not work on objects of type {$self.ref}!";
  }
}

method keys (Hash|Pair|Array $self:) {
  if $self.isa("Hash") {
    JS::inline('new PIL2JS.Box.Constant(function (args) {
      var hash = args[1].FETCH();
      var cc   = args.pop();
      var keys = hash.keys();
      cc(new PIL2JS.Box.Constant(keys));
    })')(%$self);
  } elsif $self.isa("Pair") {
    ($self.key,);
  } elsif $self.isa("Array") {
    JS::inline('new PIL2JS.Box.Constant(function (args) {
      var array = args[1].FETCH();
      var cc    = args.pop();
      var ret   = [];
      for(var i = 0; i < array.length; i++) {
        ret.push(new PIL2JS.Box.Constant(i));
      }
      cc(new PIL2JS.Box.Constant(ret));
    })')(@$self);
  } else {
    die ".keys does not work on objects of type {$self.ref}!";
  }
}

method values (Hash|Pair|Array|Junction $self:) {
  if $self.isa("Junction") {
    JS::inline('new PIL2JS.Box.Constant(function (args) {
      var junc = args[1].FETCH(), cc = args.pop();
      cc(new PIL2JS.Box.Constant(junc.values));
    })')($self);
  } elsif $self.isa("Hash") {
    # $self.keys.map:{ $self{$_} } can't work, as map returns new containers.
    # (I think.)
    my @res;
    @res[+@res] := $self{$_} for $self.keys;
    @res;
  } elsif $self.isa("Pair") {
    ($self.value,);
  } elsif $self.isa("Array") {
    @$self;
  } else {
    die ".values does not work on objects of type {$self.ref}!";
  }
}

method kv (Hash|Pair|Array $self:) {
  if $self.isa("Hash") {
    my @res;
    for $self.keys {
        push @res, $_, undef;
        @res[-1] := $self{$_};
    }
    @res;
  } elsif $self.isa("Pair") {
    ($self.key, $self.value);
  } elsif $self.isa("Array") {
    my @res;
    for $self.keys {
        push @res, $_, undef;
        @res[-1] := $self[$_];
    }
    @res;
  } else {
    die ".kv does not work on objects of type {$self.ref}!";
  }
}

method pairs (Hash|Pair|Array $self:) {
  my sub infix:«=:>» ($key, $value is rw) is primitive is rw {
    my $pair = ($key => $value);
    $pair.value := $value;
    $pair;
  }

  if $self.isa("Hash") {
    $self.keys.map:{; $_ =:> $self{$_} };
  } elsif $self.isa("Pair") {
    ($self,);
  } elsif $self.isa("Array") {
    $self.keys.map:{; $_ =:> $self[$_] };
  } else {
    die ".pairs does not work on objects of type {$self.ref}!";
  }
}

method pick (Hash|Pair|Array|Junction $self:) {
  if $self.isa("Junction") {
    my @vals = $self.values;
    @vals[int rand @vals];
  } elsif $self.isa("Hash") {
    any($self.pairs).pick;
  } elsif $self.isa("Pair") {
    $self;
  } elsif $self.isa("Array") {
    any(@$self).pick;
  }
}
