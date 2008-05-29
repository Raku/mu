
class IRx1::CompUnit {
  method do_all_analysis() {
    self.initialize_notes;
    self.note_parents;
    self.note_block_lexical_variable_decls;
    self.note_environment;
  };
};

# initialize_notes

class IRx1::Base {
  method is_IR_node(){1};
  method initialize_notes() {
    self.notes = {};

    my $a = [];
    for self.field_values {
      if $_.can('is_IR_node') {$a.push($_)}
      elsif $_.WHAT eq 'Array' {
        for $_ { 
          if $_.can('is_IR_node') {$a.push($_)}
          elsif $_.WHAT eq 'Array' {
            for $_ { 
              if $_.can('is_IR_node') {$a.push($_)}
            }
          }
        }
      };
    }
    self.notes<child_nodes> = $a;

    for self.child_nodes { $_.initialize_notes }
  };
  method destroy_notes() {
    for self.notes<child_nodes> {$_.destroy_notes}
    self.notes = undef
  };
  method child_nodes() { self.notes<child_nodes> };
};

# note_parents

class IRx1::CompUnit {
  method note_parents() {
    my $+whiteboard::parent = self;
    for self.child_nodes {$_.note_parents}
  };
};
class IRx1::Base {
  method note_parents() {
    self.notes<parent> = $+whiteboard::parent;
    my $+whiteboard::parent = self;
    for self.child_nodes {$_.note_parents}
  };
};

# note_block_lexical_variable_decls

class IRx1::CompUnit_and_Block {
  method note_block_lexical_variable_decls() {
    my $a = [];
    my $+whiteboard::lexical_variable_decls = $a;
    self.notes<lexical_variable_decls> = $a;
    for self.child_nodes {$_.note_block_lexical_variable_decls}
  };
};
class IRx1::VarDecl {
  method note_block_lexical_variable_decls() {
    if self.is_lexical {
      $+whiteboard::lexical_variable_decls.push(self);
    }
    for self.child_nodes {$_.note_block_lexical_variable_decls}
  };
};
class IRx1::SubDecl {
  method note_block_lexical_variable_decls() {
    if $_.name {
      $+whiteboard::lexical_variable_decls.push(self);
    }
    for self.child_nodes {$_.note_block_lexical_variable_decls}
  };
};
class IRx1::Base {
  method note_block_lexical_variable_decls() {
    for self.child_nodes {$_.note_block_lexical_variable_decls}
  };
};

# note_environment

class IRx1::CompUnit {
  method note_environment() {
    my $+whiteboard::package_chain = [];
    my $+whiteboard::lexical_bindings =
      self.update_lexical_bindings({},
                                   self.notes<lexical_variable_decls>);
    for self.child_nodes {$_.note_environment}
  };
};
class IRx1::Block {
  method note_environment() {
    my $+whiteboard::lexical_bindings =
      self.update_lexical_bindings($+whiteboard::lexical_bindings,
                                   self.notes<lexical_variable_decls>);
    for self.child_nodes {$_.note_environment}
  };
};
class IRx1::CompUnit_and_Block {
  method update_lexical_bindings($h,$decls) {
    my $h1 = $h.dup;
    for $decls {
      my $k = $_.sigil ~ $_.name;
      $h1{$k} = $_;
    }
    $h1;
  };
};
class IRx1::PackageDecl {
  method note_environment() {
    my $new_chain;
    if self.path_is_absolute {
      $new_chain = [self];
    } else {
      $new_chain = [$+whiteboard::package_chain.flatten,self];
    }
    my $+whiteboard::package_chain = $new_chain;
    for self.child_nodes {$_.note_environment}
  };
};
class IRx1::Apply {
  method note_environment() {
    self.notes<lexical_bindings> = $+whiteboard::lexical_bindings;
    for self.child_nodes {$_.note_environment}
  };
};
class IRx1::Var {
  method note_environment() {
    my $key = self<sigil> ~ self<name>;
    self.notes<decl> = $+whiteboard::lexical_bindings{$key};
    for self.child_nodes {$_.note_environment}
  }
}
class IRx1::Base {
  method note_environment() {
    for self.child_nodes {$_.note_environment}
  };
};


# info
class IRx1::VarDecl {
  method is_lexical() {self.scope eq 'my'};
  method is_context() {
    for self.traits {if $_.expr eq 'context' { return 1 }}
    return 0;
  }
  method name () { self.<var><name> };
  method sigil() { self.<var><sigil> };
  method is_scalar() { self.<var><sigil> eq '$' }
  method is_array() { self.<var><sigil> eq '@' }
  method is_hash() { self.<var><sigil> eq '%' }
};
class IRx1::SubDecl {
  method sigil() { '&' };
};
class IRx1::PackageDecl {
  method path_is_absolute() { self.name && self.name =~ /^GLOBAL\b'/ }
};
