# (Larry likes long names for continuations)

# sleep, slightly hacky (we redefine &exit temporarily)
sub sleep (Num $secs) is primitive {
  my $start = time;

  temp &exit = sub {};

  JS::inline('new PIL2JS.Box.Constant(function (args) {
    var secs = args[1].toNative(), cc = args.pop();

    window.setTimeout(function () {
      PIL2JS.catch_all_exceptions(function () {
        PIL2JS.catch_end_exception(function () {
          PIL2JS.runloop(function () {
            cc(new PIL2JS.Box.Constant(undefined));
          });
        });
      });

      PIL2JS.catch_all_exceptions(function () {
        PIL2JS.catch_end_exception(function () {
          PIL2JS.runloop(function () {
            _26main_3a_3aexit.FETCH()([
              PIL2JS.Context.ItemAny,
              new PIL2JS.Box.Constant(undefined),
              function () { "dummycc" }
            ]);
          });
        });
      });
    }, secs * 1000);
  })')($secs);

  time - $start;
}
