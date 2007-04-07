use v6-alpha;

###########################################################################
###########################################################################

my $EMPTY_STR = q{};

my $FALSE = (1 == 0);
my $TRUE  = (1 == 1);

###########################################################################
###########################################################################

module QDRDBMS::Engine::Example::PhysType-0.0.0 {
    # Note: This given version applies to all of this file's packages.

###########################################################################

sub Bool is export {
    return QDRDBMS::Engine::Example::PhysType::Bool.new( @_ );
}

sub Text is export {
    return QDRDBMS::Engine::Example::PhysType::Text.new( @_ );
}

sub Blob is export {
    return QDRDBMS::Engine::Example::PhysType::Blob.new( @_ );
}

sub Int is export {
    return QDRDBMS::Engine::Example::PhysType::Int.new( @_ );
}

sub TextKeyedMap is export {
    return QDRDBMS::Engine::Example::PhysType::TextKeyedMap.new( @_ );
}

sub Heading is export {
    return QDRDBMS::Engine::Example::PhysType::Heading.new( @_ );
}

sub Tuple is export {
    return QDRDBMS::Engine::Example::PhysType::Tuple.new( @_ );
}

sub Relation is export {
    return QDRDBMS::Engine::Example::PhysType::Relation.new( @_ );
}

sub Cat_DeclEntityName is export {
    return QDRDBMS::Engine::Example::PhysType::Cat_DeclEntityName.new( @_ );
}

sub Cat_InvokEntityName is export {
    return QDRDBMS::Engine::Example::PhysType::Cat_InvokEntityName.new( @_ );
}

###########################################################################

} # module QDRDBMS::Engine::Example::PhysType

###########################################################################
###########################################################################

class QDRDBMS::Engine::Example::PhysType::_Base {

    my $ATTR_WHICH = '_Base::which';

###########################################################################

method which of Str () {
    if (!exists $self.{$ATTR_WHICH}) {
        my ($cls_nm_unq_part, $scalarified_value)
            = $self._calc_parts_of_self_which();
        my $len_cnup = length $cls_nm_unq_part;
        my $len_sv = length $scalarified_value;
        $self.{$ATTR_WHICH} = '8 PhysType'
            ~ " $len_cnup $cls_nm_unq_part $len_sv $scalarified_value";
    }
    return $self.{$ATTR_WHICH};
}

###########################################################################

} # class QDRDBMS::Engine::Example::PhysType::_Base

###########################################################################
###########################################################################

class QDRDBMS::Engine::Example::PhysType::Bool {
    does QDRDBMS::Engine::Example::PhysType::_Base;

    my $ATTR_SCALAR = 'scalar';
        # A p5 Scalar that equals $FALSE|$TRUE.

###########################################################################

submethod BUILD () {
    my ($self, $scalar) = @_;
    $self.{$ATTR_SCALAR} = $scalar;
}

###########################################################################

method _calc_parts_of_self_which of  () {
    my ($self) = @_;
    return ('Bool', $self.{$ATTR_SCALAR});
}

###########################################################################

} # class QDRDBMS::Engine::Example::PhysType::Bool

###########################################################################
###########################################################################

class QDRDBMS::Engine::Example::PhysType::Text {
    does QDRDBMS::Engine::Example::PhysType::_Base;

    my $ATTR_SCALAR = 'scalar';
        # A p5 Scalar that is a text-mode string;
        # it either has true utf8 flag or is only 7-bit bytes.

###########################################################################

submethod BUILD () {
    my ($self, $scalar) = @_;
    $self.{$ATTR_SCALAR} = $scalar;
}

###########################################################################

method _calc_parts_of_self_which of  () {
    my ($self) = @_;
    return ('Text', $self.{$ATTR_SCALAR});
}

###########################################################################

} # class QDRDBMS::Engine::Example::PhysType::Text

###########################################################################
###########################################################################

class QDRDBMS::Engine::Example::PhysType::Blob {
    does QDRDBMS::Engine::Example::PhysType::_Base;

    my $ATTR_SCALAR = 'scalar';
        # A p5 Scalar that is a byte-mode string; it has false utf8 flag.

###########################################################################

submethod BUILD () {
    my ($self, $scalar) = @_;
    $self.{$ATTR_SCALAR} = $scalar;
}

###########################################################################

method _calc_parts_of_self_which of  () {
    my ($self) = @_;
    return ('Blob', $self.{$ATTR_SCALAR});
}

###########################################################################

} # class QDRDBMS::Engine::Example::PhysType::Blob

###########################################################################
###########################################################################

class QDRDBMS::Engine::Example::PhysType::Int {
    does QDRDBMS::Engine::Example::PhysType::_Base;

    use bigint; # this is experimental

    my $ATTR_SCALAR = 'scalar';
        # A p5 Scalar that is a Perl integer or BigInt or canonical string.

###########################################################################

submethod BUILD () {
    my ($self, $scalar) = @_;
    $self.{$ATTR_SCALAR} = $scalar;
}

###########################################################################

method _calc_parts_of_self_which of  () {
    my ($self) = @_;
    return ('Int', $self.{$ATTR_SCALAR});
}

###########################################################################

} # class QDRDBMS::Engine::Example::PhysType::Int

###########################################################################
###########################################################################

class QDRDBMS::Engine::Example::PhysType::TextKeyedMap {
    does QDRDBMS::Engine::Example::PhysType::_Base;

    my $ATTR_MAP = 'map';
        # A p5 Hash with 0..N elements:
            # Each Hash key is a p5 text-mode string.
            # Each Hash value is a ::Example::* value of some kind.

###########################################################################

submethod BUILD () {
    my ($self, $map) = @_;
    $self.{$ATTR_MAP} = $map;
}

###########################################################################

method _calc_parts_of_self_which of  () {
    my ($self) = @_;
    my $map = $self.{$ATTR_MAP};
    return ('TextKeyedMap', join ' ', map {
            my $mk = (length $_) ~ ' ' ~ $_;
            my $mv = $map.{$_}.which();
            "K $mk V $mv";
        } sort keys %{$map});
}

###########################################################################

method ref_to_attr_map of  () {
    my ($self) = @_;
    return $self.{$ATTR_MAP};
}

###########################################################################

method pairs of  () {
    my ($self) = @_;
    my $map = $self.{$ATTR_MAP};
    return [map { [$_, $map.{$_} ] } keys %{$map}];
}

###########################################################################

} # class QDRDBMS::Engine::Example::PhysType::TextKeyedMap

###########################################################################
###########################################################################

class QDRDBMS::Engine::Example::PhysType::Heading {
    does QDRDBMS::Engine::Example::PhysType::_Base;

    my $ATTR_ATTR_DEFS_BY_NAME = 'attr_defs_by_name';
        # A p5 Hash with 0..N elements:
            # Each Hash key is a p5 text-mode string; an attr name.
            # Each Hash value would describe a single tuple|relation
            # attribute; it is a p5 Array with 3 elements:
                # 1. attr name: a p5 text-mode string; same as Hash key.
                # 2. major type: a p5 text-mode string, one of: 'S','T','R'
                # 3. minor type: a disjunction depending on maj-tp value:
                    # 'S': a p5 text-mode string.
                    # 'T'|'R': a Heading.
    my $ATTR_ATTR_DEFS_ORDERED = 'attr_defs_ordered';
        # A p5 Array with 0..N elements; its elements are all of the Hash
        # values of $!attr_defs_by_name, sorted by the attr-name/Hash key.

###########################################################################

submethod BUILD () {
    my ($self, $attr_defs_aoa) = @_;
    my $attr_defs_by_name
        = $self.{$ATTR_ATTR_DEFS_BY_NAME}
        = {map { $_.[0] => $_ } @{$attr_defs_aoa}};
    $self.{$ATTR_ATTR_DEFS_ORDERED}
        = [map { $attr_defs_by_name.{$_} }
            sort keys %{$attr_defs_by_name}];
}

###########################################################################

method _calc_parts_of_self_which of  () {
    my ($self) = @_;
    my $defs = $self.{$ATTR_ATTR_DEFS_ORDERED};
    return ('Heading', join ' ', map {
            my ($atnm, $mjtp, $mntp) = @{$_};
            'ATNM ' ~ (length $atnm) ~ ' ' ~ $atnm
                ~ ' MJTP ' ~ (length $mjtp) ~ ' ' ~ $mjtp
                ~ ' MNTP ' ~ $mntp.which();
        } @{$defs});
}

###########################################################################

method get_attr_attr_defs_ordered of  () {
    my ($self) = @_;
    return $self.{$ATTR_ATTR_DEFS_ORDERED};
}

###########################################################################

} # class QDRDBMS::Engine::Example::PhysType::Heading

###########################################################################
###########################################################################

class QDRDBMS::Engine::Example::PhysType::Tuple {
    does QDRDBMS::Engine::Example::PhysType::_Base;

    my $ATTR_HEADING = 'heading';
        # A Heading.
    my $ATTR_BODY    = 'body';
        # A TextKeyedMap whose keys match the attribute names in $!heading,
        # and whose values are of the types specified in $!heading.

###########################################################################

submethod BUILD () {
    my ($self, $heading, $body) = @_;
    $self.{$ATTR_HEADING} = $heading;
    $self.{$ATTR_BODY}    = $body;
}

###########################################################################

method _calc_parts_of_self_which of  () {
    my ($self) = @_;
    return ('Tuple H ', $self.{$ATTR_HEADING}.which()
        ~ ' B ' ~ $self.{$ATTR_BODY}.which());
}

###########################################################################

} # class QDRDBMS::Engine::Example::PhysType::Tuple

###########################################################################
###########################################################################

class QDRDBMS::Engine::Example::PhysType::Relation {
    does QDRDBMS::Engine::Example::PhysType::_Base;

    my $ATTR_HEADING    = 'heading';
        # A Heading.
    my $ATTR_BODY       = 'body';
        # A p5 Array with 0..N elements, each element being a
        # TextKeyedMap whose keys match the attribute names in $!heading,
        # and whose values are of the types specified in $!heading.
    my $ATTR_KEY_DEFS   = 'key_defs';
        # A p5 Hash with 1..N elements
    my $ATTR_KEY_DATA   = 'key_data';
    my $ATTR_INDEX_DEFS = 'index_defs';
        # A p5 Hash with 0..N elements
    my $ATTR_INDEX_DATA = 'index_data';

###########################################################################

submethod BUILD () {
    my ($self, $heading, $body, $key_defs_aoh, $index_defs_aoh) = @_;
    # Assume input $body may contain duplicate elements (okay; silently
    # remove), and/or duplicate attributes where the attributes are keys
    # (not okay; throw an exception).
    # Otherwise assume all input is okay, and no key|index redundancy.
    my $attr_defs_ordered = $heading.get_attr_attr_defs_ordered();
    if (scalar keys %{$key_defs_aoh} == 0) {
        # There is no explicit key, so make an implicit one over all attrs.
        push @{$key_defs_aoh}, map { $_ => undef } @{$attr_defs_ordered};
    }
    my $key_defs = {};
    my $index_defs = {};




    $self.{$ATTR_HEADING}    = $heading;
    $self.{$ATTR_BODY}       = $body;
    $self.{$ATTR_KEY_DEFS}   = $key_defs;
    $self.{$ATTR_INDEX_DEFS} = $index_defs;
}

###########################################################################

method _calc_parts_of_self_which of  () {
    my ($self) = @_;
    return ('Relation H ', $self.{$ATTR_HEADING}.which()
        ~ ' B ' ~ (join ' ',
            sort map { $_.which() } @{$self.{$ATTR_BODY}}));
}

###########################################################################

} # class QDRDBMS::Engine::Example::PhysType::Relation

###########################################################################
###########################################################################

=pod

=encoding utf8

=head1 NAME

QDRDBMS::Engine::Example::PhysType -
Physical representations of all core data types

=head1 VERSION

This document describes QDRDBMS::Engine::Example::PhysType version 0.0.0.

It also describes the same-number versions of ::Bool, ::Text, ::Blob,
::Int, ::Tuple, and ::Relation.

=head1 DESCRIPTION

This file is used internally by L<QDRDBMS::Engine::Example>; it is not
intended to be used directly in user code.

It provides physical representations of data types that this Example Engine
uses to implement QDRDBMS D.  The API of these is expressly not intended to
match the API that the language itself specifies as possible
representations for system-defined data types.

Specifically, this file represents the core system-defined data types that
all QDRDBMS D implementations must have, namely: Bool, Text, Blob, Int,
Tuple, Relation, and the Cat.* types.

By contast, the optional data types are given physical representations by
other files: L<QDRDBMS::Engine::Example::PhysType::Num>,
L<QDRDBMS::Engine::Example::PhysType::Temporal>,
L<QDRDBMS::Engine::Example::PhysType::Spatial>.

=head1 BUGS AND LIMITATIONS

This file assumes that it will only be invoked by other components of
Example, and that they will only be feeding it arguments that are exactly
what it requires.  For reasons of performance, it does not do any of its
own basic argument validation, as doing so should be fully redundant.  Any
invoker should be validating any arguments that it in turn got from user
code.  Moreover, this file will often take or return values by reference,
also for performance, and the caller is expected to know that they should
not be modifying said then-shared values afterwards.

=head1 AUTHOR

Darren Duncan (C<perl@DarrenDuncan.net>)

=head1 LICENCE AND COPYRIGHT

This file is part of the QDRDBMS framework.

QDRDBMS is Copyright © 2002-2007, Darren Duncan.

See the LICENCE AND COPYRIGHT of L<QDRDBMS> for details.

=cut
