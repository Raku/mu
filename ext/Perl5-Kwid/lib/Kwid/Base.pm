package Kwid::Base;
use strict;
use warnings;
use base 'Exporter';
our @EXPORT = qw(field const);

# XXX Internal debugging only. Remove for release.
push @EXPORT, qw(XXX);
require YAML;
sub XXX {
    die YAML::Dump(@_);
}

sub new {
    my $class = shift;
    $class = ref($class) || $class;
    my $self = bless {}, $class;
    while (@_) {
        my $method = shift;
        $self->$method(shift);
    }
    return $self;    
}

# From Spiffy:
my %code = ( 
    sub_start => 
      "sub {\n  my \$self = shift;\n",
    set_default => 
      "  \$self->{%s} = %s\n    unless exists \$self->{%s};\n",
    init =>
      "  return \$self->{%s} = do { %s }\n" .
      "    unless \@_ or defined \$self->{%s};\n",
    weak_init =>
      "  return do {\n" .
      "    \$self->{%s} = do { %s };\n" .
      "    Scalar::Util::weaken(\$self->{%s}) if ref \$self->{%s};\n" .
      "    \$self->{%s};\n" .
      "  } unless \@_ or defined \$self->{%s};\n",
    return_if_get => 
      "  return \$self->{%s} unless \@_;\n",
    set => 
      "  \$self->{%s} = shift;\n",
    weaken => 
      "  Scalar::Util::weaken(\$self->{%s}) if ref \$self->{%s};\n",
    sub_end => 
      "  return \$self->{%s};\n}\n",
);

sub field {
    my $package = caller;
    my ($args, @values) = ({}, @_);
#     my ($args, @values) = do {
#         no warnings;
#         local *boolean_arguments = sub { (qw(-weak)) };
#         local *paired_arguments = sub { (qw(-package -init)) };
#         Spiffy->parse_arguments(@_);
#     };
    my ($field, $default) = @values;
    $package = $args->{-package} if defined $args->{-package};
    die "Cannot have a default for a weakened field ($field)"
        if defined $default && $args->{-weak};
    return if defined &{"${package}::$field"};
    require Scalar::Util if $args->{-weak};
    my $default_string =
        ( ref($default) eq 'ARRAY' and not @$default )
        ? '[]'
        : (ref($default) eq 'HASH' and not keys %$default )
          ? '{}'
          : default_as_code($default);

    my $code = $code{sub_start};
    if ($args->{-init}) {
        my $fragment = $args->{-weak} ? $code{weak_init} : $code{init};
        $code .= sprintf $fragment, $field, $args->{-init}, ($field) x 4;
    }
    $code .= sprintf $code{set_default}, $field, $default_string, $field
      if defined $default;
    $code .= sprintf $code{return_if_get}, $field;
    $code .= sprintf $code{set}, $field;
    $code .= sprintf $code{weaken}, $field, $field 
      if $args->{-weak};
    $code .= sprintf $code{sub_end}, $field;

    my $sub = eval $code;
    die $@ if $@;
    no strict 'refs';
    *{"${package}::$field"} = $sub;
    return $code if defined wantarray;
}

sub default_as_code {
    require Data::Dumper;
    no warnings;
    local $Data::Dumper::Sortkeys = 1;
    my $code = Data::Dumper::Dumper(shift);
    $code =~ s/^\$VAR1 = //;
    $code =~ s/;$//;
    return $code;
}

sub const {
    my $package = caller;
    my ($args, @values) = do {
        no warnings;
        local *paired_arguments = sub { (qw(-package)) };
        Spiffy->parse_arguments(@_);
    };
    my ($field, $default) = @values;
    $package = $args->{-package} if defined $args->{-package};
    no strict 'refs';
    return if defined &{"${package}::$field"};
    *{"${package}::$field"} = sub { $default }
}

1;
