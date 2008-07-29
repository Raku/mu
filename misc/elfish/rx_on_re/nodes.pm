# Warning: This file is mechanically written.  Your changes will be overwritten.
package Regexp::ModuleA::AST {

  class Pat5 is BaseClass {
    has $.match;
    has $.pat;
    has $.notes;
    
    method newp($match,$pat) { self.new('match', $match, 'pat', $pat) }
    method callback($emitter) { $emitter.cb__Pat5(self) }
    method node_name() { 'Pat5' }
    method field_names() { ['pat'] }
    method field_values() { [$.pat] }
    method irx1_describe() {
      'Pat5('~$.pat.irx1_describe~')'
    }

  }
  class Exact is BaseClass {
    has $.match;
    has $.text;
    has $.notes;
    
    method newp($match,$text) { self.new('match', $match, 'text', $text) }
    method callback($emitter) { $emitter.cb__Exact(self) }
    method node_name() { 'Exact' }
    method field_names() { ['text'] }
    method field_values() { [$.text] }
    method irx1_describe() {
      'Exact('~$.text.irx1_describe~')'
    }

  }
  class Mod_expr is Mod_Base {
    has $.match;
    has $.mods;
    has $.expr;
    has $.notes;
    
    method newp($match,$mods,$expr) { self.new('match', $match, 'mods', $mods, 'expr', $expr) }
    method callback($emitter) { $emitter.cb__Mod_expr(self) }
    method node_name() { 'Mod_expr' }
    method field_names() { ['mods','expr'] }
    method field_values() { [$.mods,$.expr] }
    method irx1_describe() {
      'Mod_expr('~$.mods.irx1_describe~','~$.expr.irx1_describe~')'
    }

  }
  class Mod_inline is Mod_Base {
    has $.match;
    has $.mods;
    has $.notes;
    
    method newp($match,$mods) { self.new('match', $match, 'mods', $mods) }
    method callback($emitter) { $emitter.cb__Mod_inline(self) }
    method node_name() { 'Mod_inline' }
    method field_names() { ['mods'] }
    method field_values() { [$.mods] }
    method irx1_describe() {
      'Mod_inline('~$.mods.irx1_describe~')'
    }

  }
  class Backref is BaseClass {
    has $.match;
    has $.backref_n;
    has $.notes;
    
    method newp($match,$backref_n) { self.new('match', $match, 'backref_n', $backref_n) }
    method callback($emitter) { $emitter.cb__Backref(self) }
    method node_name() { 'Backref' }
    method field_names() { ['backref_n'] }
    method field_values() { [$.backref_n] }
    method irx1_describe() {
      'Backref('~$.backref_n.irx1_describe~')'
    }

  }
  class Cap is BaseClass {
    has $.match;
    has $.expr;
    has $.notes;
    
    method newp($match,$expr) { self.new('match', $match, 'expr', $expr) }
    method callback($emitter) { $emitter.cb__Cap(self) }
    method node_name() { 'Cap' }
    method field_names() { ['expr'] }
    method field_values() { [$.expr] }
    method irx1_describe() {
      'Cap('~$.expr.irx1_describe~')'
    }

  }
  class Grp is BaseClass {
    has $.match;
    has $.expr;
    has $.notes;
    
    method newp($match,$expr) { self.new('match', $match, 'expr', $expr) }
    method callback($emitter) { $emitter.cb__Grp(self) }
    method node_name() { 'Grp' }
    method field_names() { ['expr'] }
    method field_values() { [$.expr] }
    method irx1_describe() {
      'Grp('~$.expr.irx1_describe~')'
    }

  }
  class Alias is BaseClass {
    has $.match;
    has $.target;
    has $.target_spec;
    has $.expr;
    has $.notes;
    
    method newp($match,$target,$target_spec,$expr) { self.new('match', $match, 'target', $target, 'target_spec', $target_spec, 'expr', $expr) }
    method callback($emitter) { $emitter.cb__Alias(self) }
    method node_name() { 'Alias' }
    method field_names() { ['target','target_spec','expr'] }
    method field_values() { [$.target,$.target_spec,$.expr] }
    method irx1_describe() {
      'Alias('~$.target.irx1_describe~','~$.target_spec.irx1_describe~','~$.expr.irx1_describe~')'
    }

  }
  class Quant is BaseClass {
    has $.match;
    has $.min;
    has $.max;
    has $.expr;
    has $.nongreedy;
    has $.notes;
    
    method newp($match,$min,$max,$expr,$nongreedy) { self.new('match', $match, 'min', $min, 'max', $max, 'expr', $expr, 'nongreedy', $nongreedy) }
    method callback($emitter) { $emitter.cb__Quant(self) }
    method node_name() { 'Quant' }
    method field_names() { ['min','max','expr','nongreedy'] }
    method field_values() { [$.min,$.max,$.expr,$.nongreedy] }
    method irx1_describe() {
      'Quant('~$.min.irx1_describe~','~$.max.irx1_describe~','~$.expr.irx1_describe~','~$.nongreedy.irx1_describe~')'
    }

  }
  class Alt is BaseClass {
    has $.match;
    has $.exprs;
    has $.notes;
    
    method newp($match,$exprs) { self.new('match', $match, 'exprs', $exprs) }
    method callback($emitter) { $emitter.cb__Alt(self) }
    method node_name() { 'Alt' }
    method field_names() { ['exprs'] }
    method field_values() { [$.exprs] }
    method irx1_describe() {
      'Alt('~$.exprs.irx1_describe~')'
    }

  }
  class Conj is BaseClass {
    has $.match;
    has $.exprs;
    has $.notes;
    
    method newp($match,$exprs) { self.new('match', $match, 'exprs', $exprs) }
    method callback($emitter) { $emitter.cb__Conj(self) }
    method node_name() { 'Conj' }
    method field_names() { ['exprs'] }
    method field_values() { [$.exprs] }
    method irx1_describe() {
      'Conj('~$.exprs.irx1_describe~')'
    }

  }
  class Seq is BaseClass {
    has $.match;
    has $.exprs;
    has $.notes;
    
    method newp($match,$exprs) { self.new('match', $match, 'exprs', $exprs) }
    method callback($emitter) { $emitter.cb__Seq(self) }
    method node_name() { 'Seq' }
    method field_names() { ['exprs'] }
    method field_values() { [$.exprs] }
    method irx1_describe() {
      'Seq('~$.exprs.irx1_describe~')'
    }

  }
  class ASpace is BaseClass {
    has $.match;
    has $.aspace_inpkg;
    has $.text;
    has $.notes;
    
    method newp($match,$aspace_inpkg,$text) { self.new('match', $match, 'aspace_inpkg', $aspace_inpkg, 'text', $text) }
    method callback($emitter) { $emitter.cb__ASpace(self) }
    method node_name() { 'ASpace' }
    method field_names() { ['aspace_inpkg','text'] }
    method field_values() { [$.aspace_inpkg,$.text] }
    method irx1_describe() {
      'ASpace('~$.aspace_inpkg.irx1_describe~','~$.text.irx1_describe~')'
    }

  }
  class Subrule is BaseClass {
    has $.match;
    has $.created_in_pkg;
    has $.name;
    has $.exprs;
    has $.neg;
    has $.nocap;
    has $.notes;
    
    method newp($match,$created_in_pkg,$name,$exprs,$neg,$nocap) { self.new('match', $match, 'created_in_pkg', $created_in_pkg, 'name', $name, 'exprs', $exprs, 'neg', $neg, 'nocap', $nocap) }
    method callback($emitter) { $emitter.cb__Subrule(self) }
    method node_name() { 'Subrule' }
    method field_names() { ['created_in_pkg','name','exprs','neg','nocap'] }
    method field_values() { [$.created_in_pkg,$.name,$.exprs,$.neg,$.nocap] }
    method irx1_describe() {
      'Subrule('~$.created_in_pkg.irx1_describe~','~$.name.irx1_describe~','~$.exprs.irx1_describe~','~$.neg.irx1_describe~','~$.nocap.irx1_describe~')'
    }

  }
  class ARegex is BaseClass {
    has $.match;
    has $.modpat;
    has $.mods;
    has $.expr;
    has $.notes;
    
    method newp($match,$modpat,$mods,$expr) { self.new('match', $match, 'modpat', $modpat, 'mods', $mods, 'expr', $expr) }
    method callback($emitter) { $emitter.cb__ARegex(self) }
    method node_name() { 'ARegex' }
    method field_names() { ['modpat','mods','expr'] }
    method field_values() { [$.modpat,$.mods,$.expr] }
    method irx1_describe() {
      'ARegex('~$.modpat.irx1_describe~','~$.mods.irx1_describe~','~$.expr.irx1_describe~')'
    }

  }
  class Biind is BaseClass {
    has $.match;
    has $.created_in_pkg;
    has $.name;
    has $.expr;
    has $.notes;
    
    method newp($match,$created_in_pkg,$name,$expr) { self.new('match', $match, 'created_in_pkg', $created_in_pkg, 'name', $name, 'expr', $expr) }
    method callback($emitter) { $emitter.cb__Biind(self) }
    method node_name() { 'Biind' }
    method field_names() { ['created_in_pkg','name','expr'] }
    method field_values() { [$.created_in_pkg,$.name,$.expr] }
    method irx1_describe() {
      'Biind('~$.created_in_pkg.irx1_describe~','~$.name.irx1_describe~','~$.expr.irx1_describe~')'
    }

  }
  class Namespace is BaseClass {
    has $.match;
    has $.created_in_pkg;
    has $.nsname;
    has $.bindings;
    has $.pkg;
    has $.notes;
    
    method newp($match,$created_in_pkg,$nsname,$bindings,$pkg) { self.new('match', $match, 'created_in_pkg', $created_in_pkg, 'nsname', $nsname, 'bindings', $bindings, 'pkg', $pkg) }
    method callback($emitter) { $emitter.cb__Namespace(self) }
    method node_name() { 'Namespace' }
    method field_names() { ['created_in_pkg','nsname','bindings','pkg'] }
    method field_values() { [$.created_in_pkg,$.nsname,$.bindings,$.pkg] }
    method irx1_describe() {
      'Namespace('~$.created_in_pkg.irx1_describe~','~$.nsname.irx1_describe~','~$.bindings.irx1_describe~','~$.pkg.irx1_describe~')'
    }

  }
  class Code is BaseClass {
    has $.match;
    has $.code;
    has $.notes;
    
    method newp($match,$code) { self.new('match', $match, 'code', $code) }
    method callback($emitter) { $emitter.cb__Code(self) }
    method node_name() { 'Code' }
    method field_names() { ['code'] }
    method field_values() { [$.code] }
    method irx1_describe() {
      'Code('~$.code.irx1_describe~')'
    }

  }
  class CodeRx is BaseClass {
    has $.match;
    has $.code;
    has $.notes;
    
    method newp($match,$code) { self.new('match', $match, 'code', $code) }
    method callback($emitter) { $emitter.cb__CodeRx(self) }
    method node_name() { 'CodeRx' }
    method field_names() { ['code'] }
    method field_values() { [$.code] }
    method irx1_describe() {
      'CodeRx('~$.code.irx1_describe~')'
    }

  }
  class Independent is BaseClass {
    has $.match;
    has $.expr;
    has $.notes;
    
    method newp($match,$expr) { self.new('match', $match, 'expr', $expr) }
    method callback($emitter) { $emitter.cb__Independent(self) }
    method node_name() { 'Independent' }
    method field_names() { ['expr'] }
    method field_values() { [$.expr] }
    method irx1_describe() {
      'Independent('~$.expr.irx1_describe~')'
    }

  }
  class Conditional is BaseClass {
    has $.match;
    has $.test;
    has $.expr_then;
    has $.expr_else;
    has $.notes;
    
    method newp($match,$test,$expr_then,$expr_else) { self.new('match', $match, 'test', $test, 'expr_then', $expr_then, 'expr_else', $expr_else) }
    method callback($emitter) { $emitter.cb__Conditional(self) }
    method node_name() { 'Conditional' }
    method field_names() { ['test','expr_then','expr_else'] }
    method field_values() { [$.test,$.expr_then,$.expr_else] }
    method irx1_describe() {
      'Conditional('~$.test.irx1_describe~','~$.expr_then.irx1_describe~','~$.expr_else.irx1_describe~')'
    }

  }
  class Lookaround is BaseClass {
    has $.match;
    has $.is_forward;
    has $.is_positive;
    has $.expr;
    has $.notes;
    
    method newp($match,$is_forward,$is_positive,$expr) { self.new('match', $match, 'is_forward', $is_forward, 'is_positive', $is_positive, 'expr', $expr) }
    method callback($emitter) { $emitter.cb__Lookaround(self) }
    method node_name() { 'Lookaround' }
    method field_names() { ['is_forward','is_positive','expr'] }
    method field_values() { [$.is_forward,$.is_positive,$.expr] }
    method irx1_describe() {
      'Lookaround('~$.is_forward.irx1_describe~','~$.is_positive.irx1_describe~','~$.expr.irx1_describe~')'
    }

  }
  class CommitSequence is BaseClass {
    has $.match;
    has $.notes;
    
    method newp($match) { self.new('match', $match) }
    method callback($emitter) { $emitter.cb__CommitSequence(self) }
    method node_name() { 'CommitSequence' }
    method field_names() { [] }
    method field_values() { [] }
    method irx1_describe() {
      'CommitSequence('~')'
    }

  }
  class CommitGroup is BaseClass {
    has $.match;
    has $.notes;
    
    method newp($match) { self.new('match', $match) }
    method callback($emitter) { $emitter.cb__CommitGroup(self) }
    method node_name() { 'CommitGroup' }
    method field_names() { [] }
    method field_values() { [] }
    method irx1_describe() {
      'CommitGroup('~')'
    }

  }
  class CommitRegex is BaseClass {
    has $.match;
    has $.notes;
    
    method newp($match) { self.new('match', $match) }
    method callback($emitter) { $emitter.cb__CommitRegex(self) }
    method node_name() { 'CommitRegex' }
    method field_names() { [] }
    method field_values() { [] }
    method irx1_describe() {
      'CommitRegex('~')'
    }

  }
  class CommitMatch is BaseClass {
    has $.match;
    has $.notes;
    
    method newp($match) { self.new('match', $match) }
    method callback($emitter) { $emitter.cb__CommitMatch(self) }
    method node_name() { 'CommitMatch' }
    method field_names() { [] }
    method field_values() { [] }
    method irx1_describe() {
      'CommitMatch('~')'
    }

  }
}
