
use v6;

class Perl::Meta::Compiler;

method compileAll ($self: Perl::Meta::Class $class) returns Void {
    $self.:compileClass($class);
    for $class.allSubclasses() -> $subclass {
        $self.:compileClass($subclass);
    }
}

method compile ($self: Perl::Meta::Class $class) returns Void {
    $self.:compileClass($class);
}

my $DEBUG = 0;

method :compileClass ($self: Perl::Meta::Class $meta) returns Void {
    my $class_code = 'class ' ~ $meta.name();
    $class_code ~= ' is ' ~ $meta.superclass().name() if $meta.superclass().defined;
    my $properties = '';
    for $meta.properties().kv() -> $label, $prop {
        $properties ~= '    has ' 
                              ~ $prop.type().name()  ~ ' ' 
                              ~ $prop.type().sigil() ~ '.' 
                              ~ $label ~ ";\n"; 
    } 
    my $methods = '
    method meta returns Perl::Meta::Class { $meta }

    method isa ($self: Str $class) returns Bool {  
        $self.meta().isATypeOf($class);
    }
    ';
    $class_code ~= " \{\n\n" ~ $properties ~ $methods ~ "\n}"; 
    say "evaling class (\n$class_code\n)\n" if $DEBUG;
    eval $class_code;
}

=pod

=head1 NAME

Perl::Meta::Compiler

=head1 SYNOPSIS

  use Perl::Meta::Compiler;
  
  my $c = Perl::Meta::Compiler.new();
  $c.compileAll($class_hierarchy);

=head1 DESCRIPTION

This currently only handles compiling class properties and some 
built in methods. I need to work out how to alias methods to 
code structures.

=head1 METHODS

=over 4

=item B<compileAll ($self: Perl::Meta::Class $class) returns Void>

=item B<compile ($self: Perl::Meta::Class $class) returns Void>

=back

=head1 AUTHORS

Stevan Little E<lt>stevan@iinteractive.comE<gt>

=cut
