
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
    temp $whiteboard::parent = self;
    for $.child_nodes {$_.note_parents}
  }
}
class IRx1::Base {
  method note_parents() {
    $.notes<parent> = $whiteboard::parent;
    temp $whiteboard::parent = self;
    for $.child_nodes {$_.note_parents}
  }
}

# note_block_lexical_variable_decls

class IRx1::CompUnit_and_Block {
  method note_block_lexical_variable_decls() {
    my $a = [];
    temp $whiteboard::lexical_variable_decls = $a;
    $.notes<lexical_variable_decls> = $a;
    for $.child_nodes {$_.note_block_lexical_variable_decls}
  }
}
class IRx1::VarDecl {
  method note_block_lexical_variable_decls() {

    if $.is_lexical {
      $whiteboard::lexical_variable_decls.push(self);
    }

    $.notes<is> = {};
    my $traits = $.traits || [];
    for $traits {
      if $_.verb eq 'is' {
        $.notes<is>{$_.expr} = 1;
      }
    }
    if (self.<var><twigil>||'') eq '+' {$.notes<is><context> = 1}
    if self.scope eq 'temp' {$.notes<is><temp> = 1}

    for $.child_nodes {$_.note_block_lexical_variable_decls}
  }
}
class IRx1::SubDecl {
  method note_block_lexical_variable_decls() {
    if $.name {
      $whiteboard::lexical_variable_decls.push(self);
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
    temp $whiteboard::package_chain = [];
    temp $whiteboard::lexical_bindings =
      $.update_lexical_bindings({},
                                $.notes<lexical_variable_decls>);
    for $.child_nodes {$_.note_environment}
  }
}
class IRx1::Block {
  method note_environment() {
    temp $whiteboard::lexical_bindings =
      $.update_lexical_bindings($whiteboard::lexical_bindings,
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
      $new_chain = [$whiteboard::package_chain.flatten,self];
    }
    temp $whiteboard::package_chain = $new_chain;
    for $.child_nodes {$_.note_environment}
  }
}
class IRx1::SubDecl {
  method note_environment() {
    $.record_crnt_package;
    for $.child_nodes {$_.note_environment}
  }
}
class IRx1::MethodDecl {
  method note_environment() {
    $.record_crnt_package;
    for $.child_nodes {$_.note_environment}
  }
}
class IRx1::VarDecl {
  method note_environment() {
    $.record_crnt_package;
    for $.child_nodes {$_.note_environment}
  }
}
class IRx1::Apply {
  method note_environment() {
    $.notes<lexical_bindings> = $whiteboard::lexical_bindings;
    for $.child_nodes {$_.note_environment}
  }
}
class IRx1::Var {
  method note_environment() {
    my $key = self<sigil> ~ self<name>;
    $.notes<decl> = $whiteboard::lexical_bindings{$key};
    if $.notes<decl> {
      $.notes<is> = $.notes<decl>.notes<is>;
    } else {
      $.notes<is> = {};
    }
    $.record_crnt_package; #Caution - only for $?PACKAGE.
    my $g = $.name.re_groups('(?:(.+)::)?([^:]+)$');
    $.notes<package> = $g[0];
    #^ TODO resolve non-absolute package names
    $.notes<bare_name> = $g[1];
    for $.child_nodes {$_.note_environment}
  }
}
class IRx1::Base {
  method record_crnt_package() {
    $.notes<crnt_package_chain> = $whiteboard::package_chain;
    #if $.notes<crnt_package_chain>.elems == 0 {$.notes<crnt_package_chain> = ['Main']} #XXX no Main PkgDecl yet.
    $.notes<crnt_package> = $.notes<crnt_package_chain>.map(sub($n){$n.name}).join("::");
    if $.notes<crnt_package_chain>.elems == 0 {$.notes<crnt_package> = 'Main'} #XXX
  }
}
class IRx1::Base {
  method note_environment() {
    for $.child_nodes {$_.note_environment}
  }
}


# info
class IRx1::VarDecl {
  method is_lexical() { $.scope eq 'my' or not($.var.package) }
  method is_context() { $.notes<is><context> }
  method is_temp() { $.notes<is><temp> }
  method name () { self.<var><name> }
  method bare_name() { self.var.bare_name }
  method package() { self.var.package }
  method crnt_package() { self.var.crnt_package }
  method sigil() { self.<var><sigil> }
  method twigil() { self.<var><twigil> }
  method is_scalar() { self.<var><sigil> eq '$' }
  method is_array() { self.<var><sigil> eq '@' }
  method is_hash() { self.<var><sigil> eq '%' }
}
class IRx1::SubDecl {
  method sigil() { '&' }
  method twigil() { '' }
  method is_scalar() { undef }
  method is_array() { undef }
  method is_hash() { undef }
}
class IRx1::Var {
  method decl() { $.notes<decl> }
  method bare_name() { self.notes<bare_name> }
  method package() { self.notes<package> }
  method crnt_package() { self.notes<crnt_package> }
  method is_context() { (self.<twigil>||'') eq '+' || $.notes<is><context> }
  method is_temp() { $.notes<is><temp> }
}
class IRx1::PackageDecl {
  method path_is_absolute() { self.name && self.name.re_matchp('^GLOBAL\b') }
}
