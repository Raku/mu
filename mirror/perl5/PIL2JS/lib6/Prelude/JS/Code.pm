method JS::Root::arity(Code $self:) {
  JS::inline('new PIL2JS.Box.Constant(function (args) {
    args.pop()(new PIL2JS.Box.Constant(args[1].FETCH().pil2js_arity));
  })')($self);
}
