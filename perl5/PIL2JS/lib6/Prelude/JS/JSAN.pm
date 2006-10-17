# XXX This sub uses JSAN internals
sub PIL2JS::Internals::use_jsan_module_imp(Str $mod, *@imports) {
  JS::inline('(function (mod, imports) {
    try {
      if(!JSAN || !JSAN._exportList || !JSAN.globalScope) { throw 1 }
    } catch(err) {
      PIL2JS.die("JSAN library not loaded or not compatible!");
    }

    var old_errorLevel = JSAN.errorLevel;
    var old_exportList = JSAN._exportList;
    try {
      JSAN.errorLevel = "die";

      // JSAN._exportList does the actual exporting.
      // We box all subs so they can be accessed without the JS:: prefix.
      // I.e. "use jsan:Number.Roman <to_roman>; say to_roman 42" works.
      // (But "say JS::to_roman 42" works, too).
      JSAN._exportList = function (classdef, exportList) {
        if (typeof(exportList) != "object") return null;
        for(var i = 0; i < exportList.length; i++) {
          var name   = exportList[i];
          // &Main::<name>
          var p6name = "_26Main_3a_3a" + name;
          JSAN.globalScope[p6name] = PIL2JS.toPIL2JSBox(classdef[name]);
        }
        // Finally, call the real _exportList.
        old_exportList.call(JSAN, classdef, exportList);
      };

      JSAN.use.apply(JSAN, [mod].concat(imports));
    } catch(err) {
      throw err;
    } finally {
      // Undo our hack.
      JSAN.errorLevel  = old_errorLevel;
      JSAN._exportList = old_exportList;
    }
  })')($mod, @imports);
}

sub PIL2JS::Internals::use_jsan_module_noimp(Str $mod) {
  # JSAN.use("Foo", []) is the same as Perl's use Foo ().
  PIL2JS::Internals::use_jsan_module_imp($mod, []);
}
