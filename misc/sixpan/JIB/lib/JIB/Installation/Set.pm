package JIB::Installation::Set;

use strict;
use warnings;
use Params::Check           qw[check];
use Log::Message::Simple    qw[:STD];
use Path::Class             ();
use Data::Dumper;

use JIB::Installation;

use base 'Object::Accessor';

=head2 $set = JIB::Installation::Set->new( [dirs => \@dirs] );

=cut

sub new {
    my $class   = shift;
    my %hash    = @_;
    
    my $dirs;
    my $tmpl = {
        dirs => { default => [@INC], strict_type => 1, store => \$dirs },
    };
    
    check( $tmpl, \%hash ) or error( Params::Check->last_error ), return;
    
    my @inst;
    for my $dir ( @$dirs ) {
        my $inst = JIB::Installation->new( dir => $dir )
            or error( "Could not create JIB::Installation object from '$dir'" );
        push @inst, $inst;
    }
    
    my $obj = $class->SUPER::new();
    $obj->mk_accessors( qw[installations] );
    
    $obj->installations( \@inst );
    
    return $obj;
}

1;

# Local variables:
# c-indentation-style: bsd
# c-basic-offset: 4
# indent-tabs-mode: nil
# End:
# vim: expandtab shiftwidth=4:
