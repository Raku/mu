package Perldoc::Parser;
use Perldoc::Base -Base;

field 'reader';
field 'receiver';
field 'parent';
field top =>
      -init => '$self->set_top';
field table =>
      -init => '$self->create_table';
const config => {
    parsers => {
        pod  => 'Perldoc::Parser::Pod',
        kwid => 'Perldoc::Parser::Kwid',
    }
};

sub parse {
    return $self->top_parse(@_)
      if ref($self) eq __PACKAGE__;
}

sub set_top {
    my $top = $self;
    $top = $_ while $top->parent;
    return $top;
}

sub top_parse {
    my $type = $self->reader->type
      or die;
    my $class = $self->config->{parsers}{$type}
      or die "Can't parse Perldoc dialect '$type'";
    eval "require $class";
    die $@ if $@;

    my $parser = $class->top_class->new(
        receiver => $self->receiver,
        reader => $self->reader,
        parent => undef,
    );
    $parser->parse;
}

sub create_parser {
    my $class = shift;
    $class->new(
        receiver => $self->receiver,
        reader => $self->reader,
        parent => $self,
    );
        
}

sub create_table {
    my $class_prefix = $self->class_prefix;
    my %table = map {
        my $class = /::/ ? $_ : "$class_prefix$_";
        $class->can('id') ? ($class->id, $class) : ();
    } $self->classes;
    \ %table;
}

package Perldoc::Parser::Block;

package Perldoc::Parser::Phrase;
