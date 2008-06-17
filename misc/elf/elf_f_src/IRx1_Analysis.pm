
class IRx1::CompUnit {
  method do_all_analysis() {
    $.initialize_notes;
    $.note_parents;
    $.note_block_lexical_variable_decls;
    $.note_environment;
  }
}

# initialize_notes
# destroy_notes

class IRx1::Base {
  method is_IR_node(){1}
  method initialize_notes() {
    $.notes = {}

    my $a = [];
    for $.field_values {
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
      }
    }
    $.notes<child_nodes> = $a;

    for $.child_nodes { $_.initialize_notes }
  }
  method destroy_notes() {
    for $.notes<child_nodes> {$_.destroy_notes}
    $.notes = undef
  }
  method child_nodes() { $.notes<child_nodes> }
}

# note_parents

class IRx1::CompUnit {
  method note_parents() {
    my $+whiteboard::parent = self;
    for $.child_nodes {$_.note_parents}
  }
}
class IRx1::Base {
  method note_parents() {
    $.notes<parent> = $+whiteboard::parent;
    my $+whiteboard::parent = self;
    for $.child_nodes {$_.note_parents}
  }
}

# note_block_lexical_variable_decls

class IRx1::CompUnit_and_Block {
  method note_block_lexical_variable_decls() {
    my $a = [];
    my $+whiteboard::lexical_variable_decls = $a;
    $.notes<lexical_variable_decls> = $a;
    for $.child_nodes {$_.note_block_lexical_variable_decls}
  }
}
class IRx1::VarDecl {
  method note_block_lexical_variable_decls() {

    if $.is_lexical {
      $+whiteboard::lexical_variable_decls.push(self);
    }

    $.notes<is> = {};
    my $traits = $.traits || [];
    for $traits {
      if $_.verb eq 'is' {
        $.notes<is>{$_.expr} = 1;
      }
    }
    if (self.<var><twigil>||'') eq '+' {$.notes<is><context> = 1}

    for $.child_nodes {$_.note_block_lexical_variable_decls}
  }
}
class IRx1::SubDecl {
  method note_block_lexical_variable_decls() {
    if $_.name {
      $+whiteboard::lexical_variable_decls.push(self);
    }
    for $.child_nodes {$_.note_block_lexical_variable_decls}
  }
}
class IRx1::Base {
  method note_block_lexical_variable_decls() {
    for $.child_nodes {$_.note_block_lexical_variable_decls}
  }
}

# note_environment

class IRx1::CompUnit {
  method note_environment() {
    my $+whiteboard::package_chain = [];
    my $+whiteboard::lexical_bindings =
      $.update_lexical_bindings({},
                                $.notes<lexical_variable_decls>);
    for $.child_nodes {$_.note_environment}
  }
}
class IRx1::Block {
  method note_environment() {
    my $+whiteboard::lexical_bindings =
      $.update_lexical_bindings($+whiteboard::lexical_bindings,
                                $.notes<lexical_variable_decls>);
    for $.child_nodes {$_.note_environment}
  }
}
class IRx1::CompUnit_and_Block {
  method update_lexical_bindings($h,$decls) {
    my $h1 = $h.dup;
    for $decls {
      my $k = $_.sigil ~ $_.name;
      $h1{$k} = $_;
    }
    $h1;
  }
}
class IRx1::PackageDecl {
  method note_environment() {
    my $new_chain;
    if $.path_is_absolute {
      $new_chain = [self];
    } else {
      $new_chain = [$+whiteboard::package_chain.flatten,self];
    }
    my $+whiteboard::package_chain = $new_chain;
    for $.child_nodes {$_.note_environment}
  }
}
class IRx1::Apply {
  method note_environment() {
    $.notes<lexical_bindings> = $+whiteboard::lexical_bindings;
    for $.child_nodes {$_.note_environment}
  }
}
class IRx1::Var {
  method note_environment() {
    my $key = self<sigil> ~ self<name>;
    $.notes<decl> = $+whiteboard::lexical_bindings{$key};
    if $.notes<decl> {
      $.notes<is> = $.notes<decl>.notes<is>;
    } else {
      $.notes<is> = {};
    }
    for $.child_nodes {$_.note_environment}
  }
}
class IRx1::Base {
  method note_environment() {
    for $.child_nodes {$_.note_environment}
  }
}


# info
class IRx1::VarDecl {
  method is_lexical() { $.scope eq 'my' }
  method is_context() { $.notes<is><context> }
  method name () { self.<var><name> }
  method sigil() { self.<var><sigil> }
  method is_scalar() { self.<var><sigil> eq '$' }
  method is_array() { self.<var><sigil> eq '@' }
  method is_hash() { self.<var><sigil> eq '%' }
}
class IRx1::SubDecl {
  method sigil() { '&' }
  method is_scalar() { undef }
  method is_array() { undef }
  method is_hash() { undef }
}
class IRx1::Var {
  method decl() { $.notes<decl> }
  method is_context() { (self.<twigil>||'') eq '+' || $.notes<is><context> }
}
class IRx1::PackageDecl {
  method path_is_absolute() { self.name && self.name =~ /^GLOBAL\b'/ }
}
