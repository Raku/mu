#line 1 "inc/Module/Install/Makefile/Name.pm - /Users/ingy/local/lib/perl5/site_perl/5.8.6/Module/Install/Makefile/Name.pm"
package Module::Install::Makefile::Name;
use Module::Install::Base; @ISA = qw(Module::Install::Base);

$VERSION = '0.01';

use strict;

sub determine_NAME {
    my $self = shift;
    my @modules = glob('*.pm');

    require File::Find;
    File::Find::find(sub { push @modules, $File::Find::name if /\.pm/i }, 'lib');

    if (@modules == 1) {
        local *MODULE;
        open MODULE, $modules[0] or die $!;
        while (<MODULE>) {
            next if /^\s*(?:#|$)/;
            $self->module_name($1) if /^\s*package\s+(\w[\w:]*)\s*;\s*$/;
            last;
        }
    }

    return if $self->module_name;

    my $name = MM->guess_name or die <<"END";
Can't determine a NAME for this distribution.
Please use the 'name' function in Makefile.PL.
END

    $name =~ s/-/::/g;
    $self->module_name($name);
    return $name;
}

1;
