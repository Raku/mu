
package Regexp::ModuleA {

  class AST::BaseClass {

    method RAST_pass10 { # flags, mods, and pkg
      self.<flags> = $Regexp::ModuleA::AST::Env::flags.clone;
      $.RAST_children.map(sub($o){$o.RAST_pass10})
    }
    method RAST_pass14 { # in_quant, subrules_seen, alias_construct
      my $q = $Regexp::ModuleA::AST::Env::in_quant;
      $q = 1 if $q; # remove 'directly'
      temp $Regexp::ModuleA::AST::Env::in_quant = $q;
      $.RAST_children.map(sub($o){$o.RAST_pass14});
    }
    method RAST_pass15 { # nparen, target_spec (req: RAST_pass14)
      $.RAST_children.map(sub($o){$o.RAST_pass15});
    }
    method RAST_pass30 {
      $.RAST_children.map(sub($o){$o.RAST_pass30})
    }

  }

  class AST::Pat5 {
  }

  class AST::Exact {
  }

  class AST::MixinMod {

    method mods_from_modpat ($modpat) { # class method
      return {} if $modpat eq '';
      my $cls = self;
      my $normalize = {
                       'perl5_i' => 'i',
                       'ignorecase' => 'i',
                       's' => 'sigspace'
                       };
      my $m = {};
      $modpat.split(":").map(sub($mod){
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
      my $flags = $Regexp::ModuleA::AST::Env::flags.clone;
      self.<mods>.keys.map(sub($key){
        $flags.{$key} = self.<mods>.{$key};
      });
      $flags;
    }
  }

  class AST::Mod_expr {
    method RAST_pass10 {
      temp $Regexp::ModuleA::AST::Env::flags = $._add_mods;
      $.SUPER::RAST_pass10;
    }
  }

  class AST::Mod_inline {
    method RAST_pass10 { # Not the same as Mod_expr's.
      $Regexp::ModuleA::AST::Env::flags = $._add_mods;
      $.SUPER::RAST_pass10;
    }
  }

  class AST::Backref {
    method RAST_pass30 {
      my $n = self.<backref_n>;
      my $total = $Regexp::ModuleA::AST::Env::nparen;
      die "Backreference to nonexistent group $n of $total"
        if $total < $n;
    }
  }

  class AST::Cap {
    method RAST_pass10 {
      self.<flags> = $Regexp::ModuleA::AST::Env::flags.clone;
      temp $Regexp::ModuleA::AST::Env::flags = $Regexp::ModuleA::AST::Env::flags.clone;
      $.SUPER::RAST_pass10;
    }
    method RAST_pass14 {
      self.<in_quant> = $Regexp::ModuleA::AST::Env::in_quant;
      temp $Regexp::ModuleA::AST::Env::in_quant = 0;
      temp $Regexp::ModuleA::AST::Env::subrules_seen = {};
      $.SUPER::RAST_pass14;
      $Regexp::ModuleA::AST::Env::alias_construct = self;
    }
    method RAST_pass15 {
      self.<cap6_idx> = $Regexp::ModuleA::AST::Env::nparen6_idx++;
      self.<cap5_idx> = $Regexp::ModuleA::AST::Env::nparen++;
      $Regexp::ModuleA::AST::Env::nparen6 = $Regexp::ModuleA::AST::Env::nparen6_idx
        if $Regexp::ModuleA::AST::Env::nparen6 < $Regexp::ModuleA::AST::Env::nparen6_idx;
      self.<target_spec> = $Regexp::ModuleA::AST::Env::target_spec;

      temp $Regexp::ModuleA::AST::Env::nparen6 = 0;
      temp $Regexp::ModuleA::AST::Env::nparen6_idx = 0;
      temp $Regexp::ModuleA::AST::Env::target_spec = undef;
      self.<expr>.RAST_pass15;
      self.<nparen6> = $Regexp::ModuleA::AST::Env::nparen6;
    }
  }

  class AST::Grp {
    method RAST_pass10 {
      self.<flags> = $Regexp::ModuleA::AST::Env::flags.clone;
      temp $Regexp::ModuleA::AST::Env::flags = $Regexp::ModuleA::AST::Env::flags.clone;
      $.SUPER::RAST_pass10;
    }
    method RAST_pass14 {
      self.<in_quant> = $Regexp::ModuleA::AST::Env::in_quant;
      $.SUPER::RAST_pass14;
      $Regexp::ModuleA::AST::Env::alias_construct = self;
    }
    method RAST_pass15 {
      self.<target_spec> = $Regexp::ModuleA::AST::Env::target_spec;
      temp $Regexp::ModuleA::AST::Env::target_spec = undef;
      self.<expr>.RAST_pass15;
    }
  }

  class AST::Alias {
    method RAST_pass14 {
      $.RAST_children.map(sub($o){$o.RAST_pass14});
      my $construct = $Regexp::ModuleA::AST::Env::alias_construct;
      my $kinds = {'Regexp::ModuleA::AST::Grp'=>'group',
                   'Regexp::ModuleA::AST::Cap'=>'capture',
                   'Regexp::ModuleA::AST::Subrule'=>'subrule'};
      my $kind = $kinds.{$construct.WHAT};
      my $in_quant = $construct.<in_quant>;
      self.<construct_kind> = $kind;
      self.<construct_in_quant> = $in_quant;
    }
    method RAST_pass15 {
      self.<first_alias> = !defined($Regexp::ModuleA::AST::Env::target_spec);
      my $spec = self.<target_spec>;
      my $idx;
      if ($spec.elems == 3 && $spec[1] eq '[') { $idx = $spec[2] }
      if defined($idx) {
        my $construct_kind = self.<construct_kind>;
        my $construct_in_quant = self.<construct_in_quant>;
        my $next_idx = $idx;
        $next_idx++ if $construct_kind eq 'group';
        $Regexp::ModuleA::AST::Env::nparen6_idx = $next_idx;
        $Regexp::ModuleA::AST::Env::nparen6 = $next_idx if
          $Regexp::ModuleA::AST::Env::nparen6 < $next_idx;
      }
      temp $Regexp::ModuleA::AST::Env::target_spec = $spec;
      self.<expr>.RAST_pass15;
    }
  }

  class AST::Quant {
    method RAST_pass14 {
      temp $Regexp::ModuleA::AST::Env::in_quant = 'directly';
      $.RAST_children.map(sub($o){$o.RAST_pass14});
    }
  }

  class AST::Alt {
    method RAST_pass15 {
      my $start = $Regexp::ModuleA::AST::Env::nparen6_idx;
      my $max = $start;
      my $x = self.<exprs>.map(sub($o){
        temp $Regexp::ModuleA::AST::Env::nparen6_idx = $start;
        my $x1 = $o.RAST_pass15;
        my $np = $Regexp::ModuleA::AST::Env::nparen6_idx;
        $max = $np if $max < $np;
        $x1;
      });
      self.<cap6_idx_start> = $start;
      self.<nparen6> = $max - $start;
      $Regexp::ModuleA::AST::Env::nparen6_idx = $max;
      $x;
    }
  }

  class AST::Conj {
  }

  class AST::Seq {
    method RAST_pass14 {
      if self.<exprs>.elems == 1 {
        # Single item sequence doesn't affect in_quant directness.
        $.RAST_children.map(sub($o){$o.RAST_pass14});
      } else {
        $.SUPER::RAST_pass14;
      }
    }
  }

  class AST::ASpace {
    method RAST_pass10 {
      my $flags = $Regexp::ModuleA::AST::Env::flags.clone;
      if $flags.<sigspace> {
        my $sr = Regexp::ModuleA::AST::Subrule.newx(self.<aspace_inpkg>,'?ws',[]);
        $.become('Regexp::ModuleA::AST::Subrule');
        # XXX anything else?
        self.<created_in_pkg> = $sr.<created_in_pkg>;
        self.<exprs> = $sr.<exprs>;
        self.<name> = $sr.<name>;
        self.<neg> = $sr.<neg>;
        self.<nocap> = $sr.<nocap>;
        self.<inpkg> = $sr.<inpkg>;
        self.<pkg> = $sr.<pkg>;
      } else {
        $.become('Regexp::ModuleA::AST::Exact');
      }
      $.RAST_pass10;
    }
  }

  class AST::Subrule {
    method RAST_pass10 {
      self.<pkg> = $Regexp::ModuleA::AST::Env::pkg || self.<inpkg>;
      $.SUPER::RAST_pass10;
    }
    method RAST_pass14 {
      self.<in_quant> = $Regexp::ModuleA::AST::Env::in_quant;
      if not(self.<nocap>) {
        my $name = self.<name>;
        my $seen = $Regexp::ModuleA::AST::Env::subrules_seen.{$name};
        if $seen {
          $seen.() if $seen.isa('Code');
          $Regexp::ModuleA::AST::Env::subrules_seen.{$name} = 1;
          self.<in_quant> = 1 if not(self.<in_quant>);
        } else {
          $Regexp::ModuleA::AST::Env::subrules_seen.{$name} = sub {
            self.<in_quant> = 1 if not(self.<in_quant>);
          };
        }
      }
      $.SUPER::RAST_pass14;
      $Regexp::ModuleA::AST::Env::alias_construct = self;
    }
    method RAST_pass15 {
      self.<target_spec> = $Regexp::ModuleA::AST::Env::target_spec;
    }
  }

  class AST::ARegex {
    method RAST_init {
      self.<pkg> = $Regexp::ModuleA::AST::Env::pkg || self.<inpkg>;
      self.<name> = $Regexp::ModuleA::AST::Env::name;
      temp $Regexp::ModuleA::AST::Env::pkg = self.<pkg>;
      temp $Regexp::ModuleA::AST::Env::flags = self.<mods>.clone;
      $.RAST_pass10;
      temp $Regexp::ModuleA::AST::Env::in_quant = 0;
      temp $Regexp::ModuleA::AST::Env::subrules_seen = {};
      temp $Regexp::ModuleA::AST::Env::alias_construct = undef;
      $.RAST_pass14;
      temp $Regexp::ModuleA::AST::Env::nparen = 0;
      temp $Regexp::ModuleA::AST::Env::nparen6 = 0;
      temp $Regexp::ModuleA::AST::Env::nparen6_idx = 0;
      temp $Regexp::ModuleA::AST::Env::target_spec = undef;
      $.RAST_pass15;
      self.<nparen> = $Regexp::ModuleA::AST::Env::nparen;
      self.<nparen6> = $Regexp::ModuleA::AST::Env::nparen6;
      $.RAST_pass30;
      self;
    }
  }

  class AST::Biind {
    method RAST_init {
      self.<pkg> = $Regexp::ModuleA::AST::Env::pkg || self.<inpkg>;
      temp $Regexp::ModuleA::AST::Env::pkg = self.<pkg>;
      temp $Regexp::ModuleA::AST::Env::name = self.<name>;
      self.<expr>.RAST_init;
      self;
    }
  }

  class AST::Namespace {
    method RAST_init {
      temp $Regexp::ModuleA::AST::Env::pkg = self.<pkg>;
      $.RAST_children.map(sub($o){$o.RAST_init});
      self;
    }
  }

  class AST::Code {
  }

  class AST::CodeRx {
  }

  class AST::Independent {
  }

  class AST::Conditional {
  }

  class AST::Lookaround {
  }


}
