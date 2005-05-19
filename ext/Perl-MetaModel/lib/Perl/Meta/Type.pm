
use v6;

class Perl::Meta::Type;

sub MkType (Str $name) returns Perl::Meta::Type is export {
    Perl::Meta::Type.new(:name($name));
}

has Str $.name;
has Str $:sigil;

submethod BUILD ($.name) {}

method sigil ($self: Str ?$sigil) returns Str {
    # define it if we got it
    if $sigil.defined {
        ($sigil ~~ rx:perl5/^[%$@&]$/)
            || die "Incorrect sigil '$sigil'";
        $:sigil = $sigil;
    }
    # if it is not defined, then define it for us
    # then the first time it is asked for, we can
    # define it using this (overly) simple table
    unless $:sigil.defined {
        given $.name {
            when 'Hash'  { $:sigil = '%' }
            when 'Array' { $:sigil = '@' }
            when 'Sub'   { $:sigil = '&' }            
            default      { $:sigil = '$' }
        }
    }
    return $:sigil;
}

=pod

=head1 NAME

Perl::Meta::Type

=head1 SYNOPSIS

  use Perl::Meta::Type;

=head1 DESCRIPTION

This is very simple String for now. Eventually it should handle Junctive types.

=head1 METHODS

=over 4

=back

=head1 AUTHORS

Stevan Little E<lt>stevan@iinteractive.comE<gt>

=cut

