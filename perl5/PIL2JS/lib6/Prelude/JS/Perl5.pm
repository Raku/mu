sub PIL2JS::Internals::use_perl5_module_imp(Str $mod, *@imports) {
  JS::inline('(
  function (mod, imports) {
    Perl5.perl_use(mod);
    for (sym in imports) {
      var f = Perl5.perl_can(mod, imports[sym]);
      // XXX: should be caller space, not main!
      // &main::<name> 
      var p6name = "_26main_3a_3a" + imports[sym];
      eval(p6name+" = PIL2JS.toPIL2JSBox(f)");
    }
})')($mod, @imports);

}

sub PIL2JS::Internals::use_perl5_module_noimp(Str $mod) {
  PIL2JS::Internals::use_perl5_module_imp($mod, []);
}
