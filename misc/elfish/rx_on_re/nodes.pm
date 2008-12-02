# Warning: This file is mechanically written.  Your changes will be overwritten.
package IRx1 {

  class RxPat5 is RxBaseClass {
    has $.match;
    has $.pat;
    has $.notes;
    
    method newp($match,$pat) { self.new('match', $match, 'pat', $pat) }
    method callback($emitter) { $emitter.cb__RxPat5(self) }
    method node_name() { 'RxPat5' }
    method field_names() { ['pat'] }
    method field_values() { [$.pat] }
    method irx1_describe() {
      'RxPat5('~$.pat.irx1_describe~')'
    }

  }
  class RxExact is RxBaseClass {
    has $.match;
    has $.text;
    has $.notes;
    
    method newp($match,$text) { self.new('match', $match, 'text', $text) }
    method callback($emitter) { $emitter.cb__RxExact(self) }
    method node_name() { 'RxExact' }
    method field_names() { ['text'] }
    method field_values() { [$.text] }
    method irx1_describe() {
      'RxExact('~$.text.irx1_describe~')'
    }

  }
  class RxMod_expr is RxMixinMod {
    has $.match;
    has $.mods;
    has $.expr;
    has $.notes;
    
    method newp($match,$mods,$expr) { self.new('match', $match, 'mods', $mods, 'expr', $expr) }
    method callback($emitter) { $emitter.cb__RxMod_expr(self) }
    method node_name() { 'RxMod_expr' }
    method field_names() { ['mods','expr'] }
    method field_values() { [$.mods,$.expr] }
    method irx1_describe() {
      'RxMod_expr('~$.mods.irx1_describe~','~$.expr.irx1_describe~')'
    }

  }
  class RxMod_inline is RxMixinMod {
    has $.match;
    has $.mods;
    has $.notes;
    
    method newp($match,$mods) { self.new('match', $match, 'mods', $mods) }
    method callback($emitter) { $emitter.cb__RxMod_inline(self) }
    method node_name() { 'RxMod_inline' }
    method field_names() { ['mods'] }
    method field_values() { [$.mods] }
    method irx1_describe() {
      'RxMod_inline('~$.mods.irx1_describe~')'
    }

  }
  class RxBackref is RxBaseClass {
    has $.match;
    has $.backref_n;
    has $.notes;
    
    method newp($match,$backref_n) { self.new('match', $match, 'backref_n', $backref_n) }
    method callback($emitter) { $emitter.cb__RxBackref(self) }
    method node_name() { 'RxBackref' }
    method field_names() { ['backref_n'] }
    method field_values() { [$.backref_n] }
    method irx1_describe() {
      'RxBackref('~$.backref_n.irx1_describe~')'
    }

  }
  class RxCap is RxBaseClass {
    has $.match;
    has $.expr;
    has $.notes;
    
    method newp($match,$expr) { self.new('match', $match, 'expr', $expr) }
    method callback($emitter) { $emitter.cb__RxCap(self) }
    method node_name() { 'RxCap' }
    method field_names() { ['expr'] }
    method field_values() { [$.expr] }
    method irx1_describe() {
      'RxCap('~$.expr.irx1_describe~')'
    }

  }
  class RxGrp is RxBaseClass {
    has $.match;
    has $.expr;
    has $.notes;
    
    method newp($match,$expr) { self.new('match', $match, 'expr', $expr) }
    method callback($emitter) { $emitter.cb__RxGrp(self) }
    method node_name() { 'RxGrp' }
    method field_names() { ['expr'] }
    method field_values() { [$.expr] }
    method irx1_describe() {
      'RxGrp('~$.expr.irx1_describe~')'
    }

  }
  class RxAlias is RxBaseClass {
    has $.match;
    has $.target;
    has $.target_spec;
    has $.expr;
    has $.notes;
    
    method newp($match,$target,$target_spec,$expr) { self.new('match', $match, 'target', $target, 'target_spec', $target_spec, 'expr', $expr) }
    method callback($emitter) { $emitter.cb__RxAlias(self) }
    method node_name() { 'RxAlias' }
    method field_names() { ['target','target_spec','expr'] }
    method field_values() { [$.target,$.target_spec,$.expr] }
    method irx1_describe() {
      'RxAlias('~$.target.irx1_describe~','~$.target_spec.irx1_describe~','~$.expr.irx1_describe~')'
    }

  }
  class RxQuant is RxBaseClass {
    has $.match;
    has $.min;
    has $.max;
    has $.expr;
    has $.nongreedy;
    has $.notes;
    
    method newp($match,$min,$max,$expr,$nongreedy) { self.new('match', $match, 'min', $min, 'max', $max, 'expr', $expr, 'nongreedy', $nongreedy) }
    method callback($emitter) { $emitter.cb__RxQuant(self) }
    method node_name() { 'RxQuant' }
    method field_names() { ['min','max','expr','nongreedy'] }
    method field_values() { [$.min,$.max,$.expr,$.nongreedy] }
    method irx1_describe() {
      'RxQuant('~$.min.irx1_describe~','~$.max.irx1_describe~','~$.expr.irx1_describe~','~$.nongreedy.irx1_describe~')'
    }

  }
  class RxAlt is RxBaseClass {
    has $.match;
    has $.exprs;
    has $.notes;
    
    method newp($match,$exprs) { self.new('match', $match, 'exprs', $exprs) }
    method callback($emitter) { $emitter.cb__RxAlt(self) }
    method node_name() { 'RxAlt' }
    method field_names() { ['exprs'] }
    method field_values() { [$.exprs] }
    method irx1_describe() {
      'RxAlt('~$.exprs.irx1_describe~')'
    }

  }
  class RxConj is RxBaseClass {
    has $.match;
    has $.exprs;
    has $.notes;
    
    method newp($match,$exprs) { self.new('match', $match, 'exprs', $exprs) }
    method callback($emitter) { $emitter.cb__RxConj(self) }
    method node_name() { 'RxConj' }
    method field_names() { ['exprs'] }
    method field_values() { [$.exprs] }
    method irx1_describe() {
      'RxConj('~$.exprs.irx1_describe~')'
    }

  }
  class RxSeq is RxBaseClass {
    has $.match;
    has $.exprs;
    has $.notes;
    
    method newp($match,$exprs) { self.new('match', $match, 'exprs', $exprs) }
    method callback($emitter) { $emitter.cb__RxSeq(self) }
    method node_name() { 'RxSeq' }
    method field_names() { ['exprs'] }
    method field_values() { [$.exprs] }
    method irx1_describe() {
      'RxSeq('~$.exprs.irx1_describe~')'
    }

  }
  class RxASpace is RxBaseClass {
    has $.match;
    has $.aspace_inpkg;
    has $.text;
    has $.notes;
    
    method newp($match,$aspace_inpkg,$text) { self.new('match', $match, 'aspace_inpkg', $aspace_inpkg, 'text', $text) }
    method callback($emitter) { $emitter.cb__RxASpace(self) }
    method node_name() { 'RxASpace' }
    method field_names() { ['aspace_inpkg','text'] }
    method field_values() { [$.aspace_inpkg,$.text] }
    method irx1_describe() {
      'RxASpace('~$.aspace_inpkg.irx1_describe~','~$.text.irx1_describe~')'
    }

  }
  class RxSubrule is RxBaseClass {
    has $.match;
    has $.created_in_pkg;
    has $.name;
    has $.exprs;
    has $.neg;
    has $.nocap;
    has $.notes;
    
    method newp($match,$created_in_pkg,$name,$exprs,$neg,$nocap) { self.new('match', $match, 'created_in_pkg', $created_in_pkg, 'name', $name, 'exprs', $exprs, 'neg', $neg, 'nocap', $nocap) }
    method callback($emitter) { $emitter.cb__RxSubrule(self) }
    method node_name() { 'RxSubrule' }
    method field_names() { ['created_in_pkg','name','exprs','neg','nocap'] }
    method field_values() { [$.created_in_pkg,$.name,$.exprs,$.neg,$.nocap] }
    method irx1_describe() {
      'RxSubrule('~$.created_in_pkg.irx1_describe~','~$.name.irx1_describe~','~$.exprs.irx1_describe~','~$.neg.irx1_describe~','~$.nocap.irx1_describe~')'
    }

  }
  class RxARegex is RxBaseClass {
    has $.match;
    has $.modpat;
    has $.mods;
    has $.expr;
    has $.notes;
    
    method newp($match,$modpat,$mods,$expr) { self.new('match', $match, 'modpat', $modpat, 'mods', $mods, 'expr', $expr) }
    method callback($emitter) { $emitter.cb__RxARegex(self) }
    method node_name() { 'RxARegex' }
    method field_names() { ['modpat','mods','expr'] }
    method field_values() { [$.modpat,$.mods,$.expr] }
    method irx1_describe() {
      'RxARegex('~$.modpat.irx1_describe~','~$.mods.irx1_describe~','~$.expr.irx1_describe~')'
    }

  }
  class RxBiind is RxBaseClass {
    has $.match;
    has $.created_in_pkg;
    has $.name;
    has $.expr;
    has $.notes;
    
    method newp($match,$created_in_pkg,$name,$expr) { self.new('match', $match, 'created_in_pkg', $created_in_pkg, 'name', $name, 'expr', $expr) }
    method callback($emitter) { $emitter.cb__RxBiind(self) }
    method node_name() { 'RxBiind' }
    method field_names() { ['created_in_pkg','name','expr'] }
    method field_values() { [$.created_in_pkg,$.name,$.expr] }
    method irx1_describe() {
      'RxBiind('~$.created_in_pkg.irx1_describe~','~$.name.irx1_describe~','~$.expr.irx1_describe~')'
    }

  }
  class RxNamespace is RxBaseClass {
    has $.match;
    has $.created_in_pkg;
    has $.nsname;
    has $.bindings;
    has $.pkg;
    has $.notes;
    
    method newp($match,$created_in_pkg,$nsname,$bindings,$pkg) { self.new('match', $match, 'created_in_pkg', $created_in_pkg, 'nsname', $nsname, 'bindings', $bindings, 'pkg', $pkg) }
    method callback($emitter) { $emitter.cb__RxNamespace(self) }
    method node_name() { 'RxNamespace' }
    method field_names() { ['created_in_pkg','nsname','bindings','pkg'] }
    method field_values() { [$.created_in_pkg,$.nsname,$.bindings,$.pkg] }
    method irx1_describe() {
      'RxNamespace('~$.created_in_pkg.irx1_describe~','~$.nsname.irx1_describe~','~$.bindings.irx1_describe~','~$.pkg.irx1_describe~')'
    }

  }
  class RxCode is RxBaseClass {
    has $.match;
    has $.code;
    has $.notes;
    
    method newp($match,$code) { self.new('match', $match, 'code', $code) }
    method callback($emitter) { $emitter.cb__RxCode(self) }
    method node_name() { 'RxCode' }
    method field_names() { ['code'] }
    method field_values() { [$.code] }
    method irx1_describe() {
      'RxCode('~$.code.irx1_describe~')'
    }

  }
  class RxCodeRx is RxBaseClass {
    has $.match;
    has $.code;
    has $.notes;
    
    method newp($match,$code) { self.new('match', $match, 'code', $code) }
    method callback($emitter) { $emitter.cb__RxCodeRx(self) }
    method node_name() { 'RxCodeRx' }
    method field_names() { ['code'] }
    method field_values() { [$.code] }
    method irx1_describe() {
      'RxCodeRx('~$.code.irx1_describe~')'
    }

  }
  class RxIndependent is RxBaseClass {
    has $.match;
    has $.expr;
    has $.notes;
    
    method newp($match,$expr) { self.new('match', $match, 'expr', $expr) }
    method callback($emitter) { $emitter.cb__RxIndependent(self) }
    method node_name() { 'RxIndependent' }
    method field_names() { ['expr'] }
    method field_values() { [$.expr] }
    method irx1_describe() {
      'RxIndependent('~$.expr.irx1_describe~')'
    }

  }
  class RxConditional is RxBaseClass {
    has $.match;
    has $.test;
    has $.expr_then;
    has $.expr_else;
    has $.notes;
    
    method newp($match,$test,$expr_then,$expr_else) { self.new('match', $match, 'test', $test, 'expr_then', $expr_then, 'expr_else', $expr_else) }
    method callback($emitter) { $emitter.cb__RxConditional(self) }
    method node_name() { 'RxConditional' }
    method field_names() { ['test','expr_then','expr_else'] }
    method field_values() { [$.test,$.expr_then,$.expr_else] }
    method irx1_describe() {
      'RxConditional('~$.test.irx1_describe~','~$.expr_then.irx1_describe~','~$.expr_else.irx1_describe~')'
    }

  }
  class RxLookaround is RxBaseClass {
    has $.match;
    has $.is_forward;
    has $.is_positive;
    has $.expr;
    has $.notes;
    
    method newp($match,$is_forward,$is_positive,$expr) { self.new('match', $match, 'is_forward', $is_forward, 'is_positive', $is_positive, 'expr', $expr) }
    method callback($emitter) { $emitter.cb__RxLookaround(self) }
    method node_name() { 'RxLookaround' }
    method field_names() { ['is_forward','is_positive','expr'] }
    method field_values() { [$.is_forward,$.is_positive,$.expr] }
    method irx1_describe() {
      'RxLookaround('~$.is_forward.irx1_describe~','~$.is_positive.irx1_describe~','~$.expr.irx1_describe~')'
    }

  }
  class RxCommitSequence is RxBaseClass {
    has $.match;
    has $.notes;
    
    method newp($match) { self.new('match', $match) }
    method callback($emitter) { $emitter.cb__RxCommitSequence(self) }
    method node_name() { 'RxCommitSequence' }
    method field_names() { [] }
    method field_values() { [] }
    method irx1_describe() {
      'RxCommitSequence('~')'
    }

  }
  class RxCommitGroup is RxBaseClass {
    has $.match;
    has $.notes;
    
    method newp($match) { self.new('match', $match) }
    method callback($emitter) { $emitter.cb__RxCommitGroup(self) }
    method node_name() { 'RxCommitGroup' }
    method field_names() { [] }
    method field_values() { [] }
    method irx1_describe() {
      'RxCommitGroup('~')'
    }

  }
  class RxCommitRegex is RxBaseClass {
    has $.match;
    has $.notes;
    
    method newp($match) { self.new('match', $match) }
    method callback($emitter) { $emitter.cb__RxCommitRegex(self) }
    method node_name() { 'RxCommitRegex' }
    method field_names() { [] }
    method field_values() { [] }
    method irx1_describe() {
      'RxCommitRegex('~')'
    }

  }
  class RxCommitMatch is RxBaseClass {
    has $.match;
    has $.notes;
    
    method newp($match) { self.new('match', $match) }
    method callback($emitter) { $emitter.cb__RxCommitMatch(self) }
    method node_name() { 'RxCommitMatch' }
    method field_names() { [] }
    method field_values() { [] }
    method irx1_describe() {
      'RxCommitMatch('~')'
    }

  }
}
