
package IRx1 {

  class RxBaseClass {

    method RAST_children { [] }
    method RAST_pass10 { # flags, mods, and pkg
      self.notes<flags> = $whiteboard::rx_flags.clone;
      $.RAST_children.map(sub ($o){$o.RAST_pass10})
    }
    method RAST_pass14 { # in_quant, subrules_seen, alias_construct
      my $q = $whiteboard::rx_in_quant;
      $q = 1 if $q; # remove 'directly'
      temp $whiteboard::rx_in_quant = $q;
      $.RAST_children.map(sub ($o){$o.RAST_pass14});
    }
    method RAST_pass15 { # nparen, target_spec (req: RAST_pass14)
      $.RAST_children.map(sub ($o){$o.RAST_pass15});
    }
    method RAST_pass30 { # backref
      $.RAST_children.map(sub ($o){$o.RAST_pass30})
    }
    method RAST_pass40 { # equivalent_re
      $.RAST_children.map(sub ($o){$o.RAST_pass40});
      $.notes<all_kids_have_equivalent_re> = 1;
      for $.RAST_children {
        $.notes<match_read> = 1 if $_.notes<match_read>;
        $.notes<match_written> = 1 if $_.notes<match_written>;
        $.notes<all_kids_have_equivalent_re> = 0 if not(defined($_.notes<equivalent_re>));
      }
    }
    method RAST_pass45 { # regex_uses_its_own_match
      temp $whiteboard::regex_uses_its_own_match = $whiteboard::regex_uses_its_own_match;
      if $whiteboard::regex_uses_its_own_match || $.notes<match_read> {
        $whiteboard::regex_uses_its_own_match = 1;
        $.notes<regex_uses_its_own_match> = 1;
      }
      $.RAST_children.map(sub ($o){$o.RAST_pass45});
    }
  }

  class RxASpace {
    method RAST_pass10 {
      self.notes<flags> = $whiteboard::rx_flags.clone;
      my $sigspace = $.notes<flags><sigspace>;
      my $p6 = not($.notes<flags><p5>);
      my $perl5_x = $.notes<flags><x>;
      my $delegate;
      if $p6 {
        if $sigspace {
          $delegate = IRx1::RxSubrule.newp(undef,self.<aspace_inpkg>,'ws',[],undef,1);
        }
      } else { #p5
        if not($perl5_x) {
          # note even if we fail to catch //x, RxExact may apply the x flag itself.
          $delegate = IRx1::RxExact.newp(undef,$.text);
        }
      }
      if not(defined($delegate)) {
        $delegate = IRx1::RxSeq.newp(undef,[]); # do nothing
      }
      $.notes<delegate> = $delegate;
      $delegate.initialize_notes;#XXX incomplete - delegate remains a mutant.
      $delegate.RAST_pass10;
    }
    method RAST_children { [$.notes<delegate>] }
    method RAST_pass14 { $.notes<delegate>.RAST_pass14 } ;#dont stomp on quant directness
    method RAST_pass40 {
      $.SUPER::RAST_pass40;
      $.notes<equivalent_re> = $.notes<delegate>.notes<equivalent_re>;
      $.notes<match_written> = $.notes<delegate>.notes<match_written>; #spec'ed as <.ws> or <ws>?
      # match_read is false
    }
  }

  class RxPat5 {
    method RAST_pass40 { 
      $.notes<equivalent_re> = $.pat;
    }
  }

  class RxExact {
    method RAST_pass40 { 
      $.notes<equivalent_re> = quotemeta($.text);
    }
  }

  class RxMixinMod {

    method mods_from_modpat ($modpat) { # class method
      return {} if $modpat eq '';
      my $cls = self;
      my $normalize = {
                       'perl5_i' => 'i',
                       'ignorecase' => 'i',
                       's' => 'sigspace'
                       };
      my $m = {};
      $modpat.split(":").map(sub ($mod){
        if $mod ne '' {
          my $g = $mod.re_groups('\A(\w+)(?:[[(<](.*?)[])>])?\z');
          die "assert" if !$g;
          my $k = $g.[0];
          my $v = $g.[1];
          $v = '1' if !defined($v);
          $v = eval_perl5($v);#XXX
          $k = $normalize.{$k} || $k;
          $m.{$k} = $v;
        }
      });
      $m;
    }

    method _add_mods {
      my $flags = $whiteboard::rx_flags.clone;
      self.mods.keys.map(sub ($key){
        $flags.{$key} = self.mods.{$key};
      });
      $flags;
    }

    method _mods_as_re {
      #XXX unfinished
      my $flags = $.notes<flags>;
      my $mods = "";
      $mods = $mods ~ "i" if $flags<i>;
      $mods;
    }
  }

  class RxMod_expr {
    method RAST_children { [$.expr] }
    method RAST_pass10 {
      temp $whiteboard::rx_flags = $._add_mods;
      $.SUPER::RAST_pass10;
    }
    method RAST_pass40 { 
      $.SUPER::RAST_pass40;
      my $re = $.expr.notes<equivalent_re>;
      my $mods = $._mods_as_re;
      if defined($re) && defined($mods) {
        $.notes<equivalent_re> = '(?'~$mods~':'~$re~')';
      }
    }
  }

  class RxMod_inline {
    method RAST_pass10 { # Not the same as Mod_expr's.
      $whiteboard::rx_flags = $._add_mods;
      $.SUPER::RAST_pass10;
    }
    method RAST_pass40 { 
      my $mods = $._mods_as_re;
      if defined($mods) {
        $.notes<equivalent_re> = '(?'~$mods~')';
      }
    }
  }

  class RxQuant {
    method RAST_children { [$.expr] }
    method RAST_pass14 {
      temp $whiteboard::rx_in_quant = 'directly';
      $.RAST_children.map(sub ($o){$o.RAST_pass14});
    }
    method RAST_pass40 {
      $.SUPER::RAST_pass40;
      my $re = $.expr.notes<equivalent_re>;
      if defined($re) && $.notes<flags><ratchet> {
        $.notes<equivalent_re> = '(?>(?:'~$re~')'~$._quantifier_as_re~')';
      }
    }
    method _quantifier_as_re {
      my $min = $.min || 0;
      my $max = $.max;
      my $q = "";
      if $max && $max == 1 && $min == 0 { $q = '?' }
      elsif $max { $q = '{'~$min~','~$max~'}' }
      elsif $min == 0 { $q = '*' }
      elsif $min == 1 { $q = '+' }
      else { die "bug" }
      if $.nongreedy { $q = $q ~ '?' }
      $q;
    }
  }

  class RxAlt {
    method RAST_children { $.exprs }
    method RAST_pass15 {
      my $start = $whiteboard::rx_nparen6_idx;
      my $max = $start;
      my $x = self.<exprs>.map(sub ($o){
        temp $whiteboard::rx_nparen6_idx = $start;
        my $x1 = $o.RAST_pass15;
        my $np = $whiteboard::rx_nparen6_idx;
        $max = $np if $max < $np;
        $x1;
      });
      self.<cap6_idx_start> = $start;
      self.<nparen6> = $max - $start;
      $whiteboard::rx_nparen6_idx = $max;
      $x;
    }
    method RAST_pass40 {
      $.SUPER::RAST_pass40;
      if $.notes<all_kids_have_equivalent_re> && $.notes<flags><ratchet> {
        my $re = $.exprs.map(sub ($o){$o.notes<equivalent_re>}).join("|");
        $.notes<equivalent_re> = '(?>'~$re~')';
      }
    }
  }

  class RxConj {
    method RAST_children { $.exprs }
    method RAST_pass40 {
      $.SUPER::RAST_pass40;
      #X unimplemented
    }
  }

  class RxSeq {
    method RAST_children { $.exprs }
    method RAST_pass14 {
      if self.<exprs>.elems == 1 {
        # Single item sequence doesn't affect in_quant directness.
        $.RAST_children.map(sub ($o){$o.RAST_pass14});
      } else {
        $.SUPER::RAST_pass14;
      }
    }
    method RAST_pass40 {
      $.SUPER::RAST_pass40;
      if $.notes<all_kids_have_equivalent_re> && $.notes<flags><ratchet> {
        my $re = $.exprs.map(sub ($o){$o.notes<equivalent_re>}).join("");
        $.notes<equivalent_re> = $re;
      }
    }
  }

  class RxAlias {
    method RAST_children { [$.expr] }
    method RAST_pass14 {
      $.RAST_children.map(sub ($o){$o.RAST_pass14});
      my $construct = $whiteboard::rx_alias_construct;
      my $kinds = {'IRx1::RxGrp'=>'group',
                   'IRx1::RxCap'=>'capture',
                   'IRx1::RxSubrule'=>'subrule'};
      my $kind = $kinds.{$construct.WHAT};
      my $in_quant = $construct.notes<in_quant>;
      self.<construct_kind> = $kind;
      self.<construct_in_quant> = $in_quant;
    }
    method RAST_pass15 {
      self.<first_alias> = !defined($whiteboard::rx_target_spec);
      self.notes<target_spec> = $._create_target_spec_from_target(self.<target>);
      my $spec = self.notes<target_spec>;
      my $idx;
      if ($spec.elems == 3 && $spec[1] eq '[') { $idx = $spec[2] }
      if defined($idx) {
        my $construct_kind = self.<construct_kind>;
        my $construct_in_quant = self.<construct_in_quant>;
        my $next_idx = $idx;
        $next_idx++ if $construct_kind eq 'group';
        $whiteboard::rx_nparen6_idx = $next_idx;
        $whiteboard::rx_nparen6 = $next_idx if
          $whiteboard::rx_nparen6 < $next_idx;
      }
      temp $whiteboard::rx_target_spec = $spec;
      self.<expr>.RAST_pass15;
    }
    method _create_target_spec_from_target ($target) {
      my $g = $target.re_groups('^([\$\@\%](?:[[:alpha:]_][\w:]+|\/)?)(.*)$');
      if not($g) { die "bug" }
      my $root = $g[0];
      my $rest = $g[1];
      my $parts = [];
      if ($g = $rest.re_groups('^(\d+)(.*)')) {
        $parts.push('[',$g[0]);
        $rest = $g[1];
      }
      while ($rest ne "") {
        $g = $rest.re_groups('^((<)(\w+)>|(\[)(\d+)\])(.*)');
        if not($g) { die "bug" }
        my $key = $g[2] || $g[4];
        my $kind = $g[1] || $g[3];
        my %normalize = ('['=>'[','{'=>'{','<'=>'{');
        $kind = %normalize{$kind};
        $parts.push($kind,$key);
        $rest = $g[5];
      }
      $root = $root ~ '/' if $root.chars == 1;
      $parts.unshift($root);
      my $target_spec = $parts;
      return $target_spec;
    }
    method RAST_pass40 {
      $.SUPER::RAST_pass40;
      $.notes<match_written> = 1;
      $.notes<equivalent_re> = $.expr.notes<equivalent_re>;
    }
  }

  class RxCap {
    method RAST_children { [$.expr] }
    method RAST_pass10 {
      self.notes<flags> = $whiteboard::rx_flags.clone;
      temp $whiteboard::rx_flags = $whiteboard::rx_flags.clone;
      $.SUPER::RAST_pass10;
    }
    method RAST_pass14 {
      self.notes<in_quant> = $whiteboard::rx_in_quant;
      temp $whiteboard::rx_in_quant = 0;
      temp $whiteboard::rx_subrules_seen = {};
      $.SUPER::RAST_pass14;
      $whiteboard::rx_alias_construct = self;
    }
    method RAST_pass15 {
      self.notes<cap6_idx> = $whiteboard::rx_nparen6_idx++;
      self.notes<cap5_idx> = $whiteboard::rx_nparen++;
      $whiteboard::rx_nparen6 = $whiteboard::rx_nparen6_idx
        if $whiteboard::rx_nparen6 < $whiteboard::rx_nparen6_idx;
      self.notes<target_spec> = $whiteboard::rx_target_spec;

      temp $whiteboard::rx_nparen6 = 0;
      temp $whiteboard::rx_nparen6_idx = 0;
      temp $whiteboard::rx_target_spec = undef;
      self.<expr>.RAST_pass15;
      self.<nparen6> = $whiteboard::rx_nparen6;
    }
    method RAST_pass40 {
      $.SUPER::RAST_pass40;
      $.notes<match_written> = 1;
      my $re = $.expr.notes<equivalent_re>;
      if defined($re) {
        $.notes<equivalent_re> = '(?:'~$re~')'; # (?:a) not (a)
      }
    }
  }

  class RxGrp {
    method RAST_children { [$.expr] }
    method RAST_pass10 {
      self.notes<flags> = $whiteboard::rx_flags.clone;
      temp $whiteboard::rx_flags = $whiteboard::rx_flags.clone;
      $.SUPER::RAST_pass10;
    }
    method RAST_pass14 {
      self.notes<in_quant> = $whiteboard::rx_in_quant;
      $.SUPER::RAST_pass14;
      $whiteboard::rx_alias_construct = self;
    }
    method RAST_pass15 {
      self.notes<target_spec> = $whiteboard::rx_target_spec;
      temp $whiteboard::rx_target_spec = undef;
      self.<expr>.RAST_pass15;
    }
    method RAST_pass40 {
      $.SUPER::RAST_pass40;
      my $re = $.expr.notes<equivalent_re>;
      if defined($re) {
        $.notes<equivalent_re> = '(?:'~$re~')';
      }
    }
  }

  class RxBackref {
    method RAST_pass30 {
      my $n = self.backref_n;
      my $total = $whiteboard::rx_nparen;
      die "Backreference to nonexistent group $n of $total"
        if $total <= $n;
    }
    method RAST_pass40 {
      $.notes<match_read> = 1;
    }
  }

  class RxSubrule {
    method RAST_children { $.exprs }
    method RAST_pass10 {
      self.<pkg> = $whiteboard::rx_pkg || self.<inpkg>;
      $.SUPER::RAST_pass10;
    }
    method RAST_pass14 {
      self.notes<in_quant> = $whiteboard::rx_in_quant;
      if not(self.<nocap>) {
        my $name = self.<name>;
        my $seen = $whiteboard::rx_subrules_seen.{$name};
        if $seen {
          $seen.() if $seen.isa('Code');
          $whiteboard::rx_subrules_seen.{$name} = 1;
          self.notes<in_quant> = 1 if not(self.notes<in_quant>);
        } else {
          $whiteboard::rx_subrules_seen.{$name} = sub {
            self.notes<in_quant> = 1 if not(self.notes<in_quant>);
          };
        }
      }
      $.SUPER::RAST_pass14;
      $whiteboard::rx_alias_construct = self;
    }
    method RAST_pass15 {
      self.notes<target_spec> = $whiteboard::rx_target_spec;
    }
    method RAST_pass40 {
      $.SUPER::RAST_pass40;
      $.notes<match_written> = 1 if not($.nocap);
    }
  }

  class RxConditional {
    method RAST_children {
      my $a = [];
      my $test = $.test;
      $a.push($test) if $test.isa("IRx1::RxBaseClass");#X (was !/\A\d+\z/)
      $a.push($.expr_then);
      my $expr_else = $.expr_else;
      $a.push($expr_else) if defined($expr_else);
      $a;
    }
    method RAST_pass40 {
      $.SUPER::RAST_pass40;
      $.notes<match_read> = 1; #X only sometimes
    }
  }

  class RxLookaround {
    method RAST_children { [$.expr] }
    method RAST_pass40 {
      $.SUPER::RAST_pass40;
      my $re = $.expr.notes<equivalent_re>;
      if defined($re) {
        my $x1; my $x2;
        if $.is_forward { $x1 = "" } else { $x1 = "<" }
        if $.is_positive { $x2 = "=" } else { $x2 = "!" }
        $.notes<equivalent_re> = '(?'~$x1~$x2~$re~')';
      }
    }
  }

  class RxIndependent {
    method RAST_children { [$.expr] }
    method RAST_pass40 {
      $.SUPER::RAST_pass40;
      my $re = $.expr.notes<equivalent_re>;
      if defined($re) {
        $.notes<equivalent_re> = '(?>'~$re~')';
      }
    }
  }

  class RxCommitSequence {
  }

  class RxCommitGroup {
  }

  class RxCommitRegex {
  }

  class RxCommitMatch {
  }

  class RxCode {
    method RAST_pass40 {
      $.notes<match_read> = 1; #X only sometimes
      $.notes<match_written> = 1; #X only sometimes
    }
  }

  class RxCodeRx {
    method RAST_pass40 {
      $.notes<match_read> = 1; #X only sometimes
      $.notes<match_written> = 1; #X only sometimes
    }
  }

  class RxARegex {
    method RAST_children { [$.expr] }
    method RAST_init {
      if !defined(self.notes) { self.initialize_notes }; #X for test
      self.<pkg> = $whiteboard::rx_pkg || self.<inpkg>;
      self.<name> = $whiteboard::rx_name;
      temp $whiteboard::rx_pkg = self.<pkg>;
      temp $whiteboard::rx_flags = self.mods.clone;
      $.RAST_pass10;
      temp $whiteboard::rx_in_quant = 0;
      temp $whiteboard::rx_subrules_seen = {};
      temp $whiteboard::rx_alias_construct = undef;
      $.RAST_pass14;
      temp $whiteboard::rx_nparen = 0;
      temp $whiteboard::rx_nparen6 = 0;
      temp $whiteboard::rx_nparen6_idx = 0;
      temp $whiteboard::rx_target_spec = undef;
      $.RAST_pass15;
      self.<nparen> = $whiteboard::rx_nparen;
      self.<nparen6> = $whiteboard::rx_nparen6;
      $.RAST_pass30;
      $.RAST_pass40;
      $.RAST_pass45;
      self;
    }
    method RAST_pass40 {
      $.SUPER::RAST_pass40;
      $.notes<equivalent_re> = $.expr.notes<equivalent_re>;
    }
  }

  class RxBiind {
    method RAST_children { [$.expr] }
    method RAST_init {
      if !defined(self.notes) { self.initialize_notes }; #X for test
      self.<pkg> = $whiteboard::rx_pkg || self.<inpkg>;
      temp $whiteboard::rx_pkg = self.<pkg>;
      temp $whiteboard::rx_name = self.<name>;
      self.<expr>.RAST_init;
      self;
    }
  }

  class RxNamespace {
    method RAST_children { [$.bindings.flatten] }
    method RAST_init {
      if !defined(self.notes) { self.initialize_notes }; #X for test
      temp $whiteboard::rx_pkg = self.<pkg>;
      $.RAST_children.map(sub ($o){$o.RAST_init});
      self;
    }
  }

}
