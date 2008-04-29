package Data::Capture;
our $VERSION = '0.25';
use 5.006;
use strict;
use warnings;
use Scalar::Util qw( refaddr blessed );

sub new {
	my ( $class, @params ) = @_;
	if ( @params == 1 ) {
		return bless $params[0], $class;
	} else {
		return bless { invocant => \(my $x), positional => [], named => { }, @params }, $class;
	}
}

sub data       { $_[0]             }
sub scalar     { $_[0]{invocant}   } # so $$c gets invocant
sub array      { $_[0]{positional} }
sub hash       { $_[0]{named}      }
sub invocant   { $_[0]{invocant}   }
sub positional { $_[0]{positional} }
sub named      { $_[0]{named}      }

sub keys   { CORE::keys   %{$_[0]{named}} }
sub values { CORE::values %{$_[0]{named}} }

sub kv     { map { ( $_, $_[0]{$_} ) }   $_[0]->keys  }

sub elems  { scalar $_[0]->keys }

sub perl {
    require Data::Dumper;
    local $Data::Dumper::Terse    = 1;
    local $Data::Dumper::Sortkeys = 1;
    local $Data::Dumper::Pad = '  ';
    return __PACKAGE__ . "->new( " . Dumper( $_[0]->data ) . ")\n";
}

sub yaml {
    require YAML::Syck;
    # interoperability with other YAML/Syck bindings:
    $YAML::Syck::ImplicitTyping = 1;
    YAML::Syck::Dump( $_[0] );
}

1;

__END__

=head1 NAME 

Data::Capture - Perl6 Capture objects

=head1 METHODS

=head1 SEE ALSO

L<Data::Bind>

=head1 AUTHORS

The Pugs Team E<lt>perl6-compiler@perl.orgE<gt>.

=head1 COPYRIGHT

Copyright 2006 by Chia-liang Kao and others.

This program is free software; you can redistribute it and/or modify it
under the same terms as Perl itself.

See L<http://www.perl.com/perl/misc/Artistic.html>

=cut

