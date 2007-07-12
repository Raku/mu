use warnings;use strict;
package KindaPerl6::Runtime::Perl5::DispatchSugar;
use Exporter "import";
our @EXPORT = qw(sugar sugar_off);
sub sugar {
    my $ref = shift;
    bless($ref,"KindaPerl6::Runtime::Perl5::DispatchSugar::Dispatch");
    return $ref;
}
sub sugar_off {
    warn "TODO";
}
package KindaPerl6::Runtime::Perl5::DispatchSugar::Dispatch;
sub AUTOLOAD {
    our $AUTOLOAD;
    $AUTOLOAD =~ s/.*:://;
    my ($self,@args) = @_;
    #use Data::Dump::Streamer;
    #print Dump($self);
    $self->{_dispatch}($self,$AUTOLOAD,@args);
}
sub DESTROY {
}
