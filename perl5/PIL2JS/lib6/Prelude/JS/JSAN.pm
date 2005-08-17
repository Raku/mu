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
      JSAN._exportList = function (classdef, exportList) {
        for(var i = 0; i < exportList.length; i++) {
          var name   = exportList[i];
          var p6name = "_26main_3a_3a" + name;
          JSAN.globalScope[p6name] = PIL2JS.toPIL2JSBox(classdef[name]);
        }
        old_exportList.call(JSAN, classdef, exportList);
      };
    
      JSAN.use.apply(JSAN, [mod].concat(imports));
    } catch(err) {
      throw err;
    } finally {
      JSAN.errorLevel  = old_errorLevel;
      JSAN._exportList = old_exportList;
    }
  })')($mod, @imports);
}

# XXX PIL2JS::Internals::use_jsan_module_noimp
