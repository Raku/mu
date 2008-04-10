
class IRx1::CompUnit {
  method do_all_analysis() {
    self.initialize_notes;
    self.note_parents;
    self.note_block_lexical_variable_decls;
  };
};

# initialize_notes

class IRx1::Base {
  method is_IR_node(){1};
  method initialize_notes() {
    self.notes({});

    my $a = [];
    for self.field_values {
      if $_.can('is_IR_node') {$a.push($_)}
      elsif $_.WHAT eq 'ARRAY' {
        for $_ { 
          if $_.can('is_IR_node') {$a.push($_)}
          elsif $_.WHAT eq 'ARRAY' { # for Cond
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
    self.notes(undef)
  };
  method child_nodes() { self.notes<child_nodes> };
};

# note_parents

class IRx1::CompUnit {
  method note_parents() {
    my $^whiteboard::parent = self;
    for self.child_nodes {$_.note_parents}
  };
};
class IRx1::Base {
  method note_parents() {
    self.notes<parent> = $^whiteboard::parent;
    my $^whiteboard::parent = self;
    for self.child_nodes {$_.note_parents}
  };
};

# note_block_lexical_variable_decls

class IRx1::CompUnit_and_Block {
  method note_block_lexical_variable_decls() {
    my $a = [];
    my $^whiteboard::lexical_variable_decls = $a;
    self.notes<lexical_variable_decls> = $a;
    for self.child_nodes {$_.note_block_lexical_variable_decls}
  };
};
class IRx1::VarDecl {
  method note_block_lexical_variable_decls() {
    if self.is_lexical {
      $^whiteboard::lexical_variable_decls.push(self);
    }
  };
};
class IRx1::Base {
  method note_block_lexical_variable_decls() {
    for self.child_nodes {$_.note_block_lexical_variable_decls}
  };
};

# VarDecl
class IRx1::VarDecl {
  method is_lexical() {self.scope eq 'my'};
};

