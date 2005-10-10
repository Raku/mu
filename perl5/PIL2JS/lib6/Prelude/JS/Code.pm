method JS::Root::arity(Code $self:) {
  JS::inline('new PIL2JS.Box.Constant(function (args) {
    args.pop()(new PIL2JS.Box.Constant(args[1].FETCH().pil2js_arity));
  })')($self);
}

method name(Code $self:) {
  JS::inline('new PIL2JS.Box.Constant(function (args) {
    args.pop()(new PIL2JS.Box.Constant(args[1].FETCH().pil2js_name));
  })')($self);
}

# XXX waiting for luqui's tuple type:
# method assuming(Code $self: *$args) {
#   return sub (*$new_args) {
#     $self(*$args, *$new_args);
#   };
# }
method assuming(Code $self: *@args) {
  return sub (*@new_args) {
    $self(*@args, *@new_args);
  };
}

sub lazy (Code $fetch) is primitive {
  JS::inline('new PIL2JS.Box.Constant(function (args) {
    var fetch = args[1].FETCH(), cc = args.pop();

    // Firstly, we create a boxed undefined. This will later become our proxy.
    var boxed = new PIL2JS.Box(undefined);

    // Then we can setup the reference.
    var ref       = new PIL2JS.Ref(boxed);
    ref.autoderef = true;
    ref           = new PIL2JS.Box(ref);

    // Finally we fix boxed.FETCH and boxed.STORE.
    // We had to delay this because PIL2JS.Ref calls .FETCH() (to init its
    // .container_type), but this is clearly not what we want (remember,
    // we\'re implementing *lazy*).
    var cached_result;
    var already_calced = false;

    boxed.FETCH  = function ()  {
      if(!already_calced) {
        cached_result = PIL2JS.cps2normal(
          fetch,
          [PIL2JS.Context.ItemAny]
        ).FETCH();
        already_calced = true;
      }

      return cached_result;
    };

    boxed.STORE  = function (n) { PIL2JS.die("Can\'t modify lazy {...} value!") };
    boxed.BINDTO = function (n) { PIL2JS.die("Can\'t rebind lazy {...} value!") };
    ref.STORE    = function (n) { PIL2JS.die("Can\'t modify lazy {...} value!") };
    ref.BINDTO   = function (n) { PIL2JS.die("Can\'t rebind lazy {...} value!") };

    cc(ref);
  })')($fetch);
}
