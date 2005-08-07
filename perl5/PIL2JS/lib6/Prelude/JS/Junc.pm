for <any all none one> -> $type {
  Pugs::Internals::eval "
    sub JS::Root::$type (*\@vals) \{
      JS::inline('new PIL2JS.Box.Constant(function (args) \{
        return new PIL2JS.Box.Constant(
          new PIL2JS.Junction.{ucfirst $type}(args[1].FETCH())
        );
      \})')(\@vals);
    \}
  ";
}

our &infix:<|> = &JS::Root::any;
our &infix:<&> = &JS::Root::all;
our &infix:<^> = &JS::Root::one;
