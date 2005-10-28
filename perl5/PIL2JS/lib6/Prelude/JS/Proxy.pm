sub PIL2JS::Internals::new_proxy (
  Code :$FETCH = { die "No FETCH routine supplied" },
  Code :$STORE = { die "No STORE routine supplied" },
) is rw {
  JS::inline('new PIL2JS.Box.Constant(function (args) {
    var fetch = args[1].FETCH(), store = args[2].FETCH(), cc = args.pop();

    var proxy = new PIL2JS.Box.Proxy(
      function () {
        return PIL2JS.cps2normal(fetch, [ PIL2JS.Context.ItemAny ]).FETCH();
      },
      function (n) {
        PIL2JS.cps2normal(store, [ PIL2JS.Context.ItemAny, n ]);
        return this;
      }
    );

    cc(proxy);
  })')($FETCH, $STORE);
}
