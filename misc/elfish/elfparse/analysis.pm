
package IRx1 {

  class RxBaseClass {

    method RAST_pass10 { # flags, mods, and pkg
      self.<flags> = $whiteboard::rx_flags.clone;
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
    method RAST_pass30 {
      $.RAST_children.map(sub ($o){$o.RAST_pass30})
    }

  }

  class RxPat5 {
  }

  class RxExact {
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
      self.<mods>.keys.map(sub ($key){
        $flags.{$key} = self.<mods>.{$key};
      });
      $flags;
    }
  }

  class RxMod_expr {
    method RAST_pass10 {
      temp $whiteboard::rx_flags = $._add_mods;
      $.SUPER::RAST_pass10;
    }
  }

  class RxMod_inline {
    method RAST_pass10 { # Not the same as Mod_expr's.
      $whiteboard::rx_flags = $._add_mods;
      $.SUPER::RAST_pass10;
    }
  }

  class RxBackref {
    method RAST_pass30 {
      my $n = self.<backref_n>;
      my $total = $whiteboard::rx_nparen;
      die "Backreference to nonexistent group $n of $total"
        if $total < $n;
    }
  }

  class RxCap {
    method RAST_pass10 {
      self.<flags> = $whiteboard::rx_flags.clone;
      temp $whiteboard::rx_flags = $whiteboard::rx_flags.clone;
      $.SUPER::RAST_pass10;
    }
    method RAST_pass14 {
      self.<in_quant> = $whiteboard::rx_in_quant;
      temp $whiteboard::rx_in_quant = 0;
      temp $whiteboard::rx_subrules_seen = {};
      $.SUPER::RAST_pass14;
      $whiteboard::rx_alias_construct = self;
    }
    method RAST_pass15 {
      self.<cap6_idx> = $whiteboard::rx_nparen6_idx++;
      self.<cap5_idx> = $whiteboard::rx_nparen++;
      $whiteboard::rx_nparen6 = $whiteboard::rx_nparen6_idx
        if $whiteboard::rx_nparen6 < $whiteboard::rx_nparen6_idx;
      self.<target_spec> = $whiteboard::rx_target_spec;

      temp $whiteboard::rx_nparen6 = 0;
      temp $whiteboard::rx_nparen6_idx = 0;
      temp $whiteboard::rx_target_spec = undef;
      self.<expr>.RAST_pass15;
      self.<nparen6> = $whiteboard::rx_nparen6;
    }
  }

  class RxGrp {
    method RAST_pass10 {
      self.<flags> = $whiteboard::rx_flags.clone;
      temp $whiteboard::rx_flags = $whiteboard::rx_flags.clone;
      $.SUPER::RAST_pass10;
    }
    method RAST_pass14 {
      self.<in_quant> = $whiteboard::rx_in_quant;
      $.SUPER::RAST_pass14;
      $whiteboard::rx_alias_construct = self;
    }
    method RAST_pass15 {
      self.<target_spec> = $whiteboard::rx_target_spec;
      temp $whiteboard::rx_target_spec = undef;
      self.<expr>.RAST_pass15;
    }
  }

  class RxAlias {
    method RAST_pass14 {
      $.RAST_children.map(sub ($o){$o.RAST_pass14});
      my $construct = $whiteboard::rx_alias_construct;
      my $kinds = {'IRx1::RxGrp'=>'group',
                   'IRx1::RxCap'=>'capture',
                   'IRx1::RxSubrule'=>'subrule'};
      my $kind = $kinds.{$construct.WHAT};
      my $in_quant = $construct.<in_quant>;
      self.<construct_kind> = $kind;
      self.<construct_in_quant> = $in_quant;
    }
    method RAST_pass15 {
      self.<first_alias> = !defined($whiteboard::rx_target_spec);
      my $spec = self.<target_spec>;
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
  }

  class RxQuant {
    method RAST_pass14 {
      temp $whiteboard::rx_in_quant = 'directly';
      $.RAST_children.map(sub ($o){$o.RAST_pass14});
    }
  }

  class RxAlt {
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
  }

  class RxConj {
  }

  class RxSeq {
    method RAST_pass14 {
      if self.<exprs>.elems == 1 {
        # Single item sequence doesn't affect in_quant directness.
        $.RAST_children.map(sub ($o){$o.RAST_pass14});
      } else {
        $.SUPER::RAST_pass14;
      }
    }
  }

  class RxASpace {
    method RAST_pass10 {
      my $flags = $whiteboard::rx_flags.clone;
      if $flags.<sigspace> {
        my $sr = IRx1::RxSubrule.newx(self.<aspace_inpkg>,'?ws',[]);
        $.become('IRx1::RxSubrule');
        # XXX anything else?
        self.<created_in_pkg> = $sr.<created_in_pkg>;
        self.<exprs> = $sr.<exprs>;
        self.<name> = $sr.<name>;
        self.<neg> = $sr.<neg>;
        self.<nocap> = $sr.<nocap>;
        self.<inpkg> = $sr.<inpkg>;
        self.<pkg> = $sr.<pkg>;
      } else {
        $.become('IRx1::RxExact');
      }
      $.RAST_pass10;
    }
  }

  class RxSubrule {
    method RAST_pass10 {
      self.<pkg> = $whiteboard::rx_pkg || self.<inpkg>;
      $.SUPER::RAST_pass10;
    }
    method RAST_pass14 {
      self.<in_quant> = $whiteboard::rx_in_quant;
      if not(self.<nocap>) {
        my $name = self.<name>;
        my $seen = $whiteboard::rx_subrules_seen.{$name};
        if $seen {
          $seen.() if $seen.isa('Code');
          $whiteboard::rx_subrules_seen.{$name} = 1;
          self.<in_quant> = 1 if not(self.<in_quant>);
        } else {
          $whiteboard::rx_subrules_seen.{$name} = sub {
            self.<in_quant> = 1 if not(self.<in_quant>);
          };
        }
      }
      $.SUPER::RAST_pass14;
      $whiteboard::rx_alias_construct = self;
    }
    method RAST_pass15 {
      self.<target_spec> = $whiteboard::rx_target_spec;
    }
  }

  class RxARegex {
    method RAST_init {
      self.<pkg> = $whiteboard::rx_pkg || self.<inpkg>;
      self.<name> = $whiteboard::rx_name;
      temp $whiteboard::rx_pkg = self.<pkg>;
      temp $whiteboard::rx_flags = self.<mods>.clone;
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
      self;
    }
  }

  class RxBiind {
    method RAST_init {
      self.<pkg> = $whiteboard::rx_pkg || self.<inpkg>;
      temp $whiteboard::rx_pkg = self.<pkg>;
      temp $whiteboard::rx_name = self.<name>;
      self.<expr>.RAST_init;
      self;
    }
  }

  class RxNamespace {
    method RAST_init {
      temp $whiteboard::rx_pkg = self.<pkg>;
      $.RAST_children.map(sub ($o){$o.RAST_init});
      self;
    }
  }

  class RxCode {
  }

  class RxCodeRx {
  }

  class RxIndependent {
  }

  class RxConditional {
  }

  class RxLookaround {
  }


}
