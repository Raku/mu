# This package pre-fill the Grammar::Perl6 package
package Grammar::Perl6Init;

use warnings;
use Pugs::Runtime::Match;
no warnings qw(once);

sub p6ws {
    my $grammar = shift;
    $_[0] = "" unless defined $_[0];
    my $bool = $_[0] =~ /^((?:\s|\#(?-s:.)*)+)(.*)$/sx;
    return Pugs::Runtime::Match->new({
        bool  => $bool,
        match => $1,
        tail  => $2,
        #capture => $1,
    });
}
sub varscalar {
    my $grammar =  shift;
    $_[0] = "" unless defined $_[0];
    my $bool = $_[0] =~ / ^(\$(?:(?:\:\:)?[_[:alnum:]]+)+)(.*)$ /xs;
    return Pugs::Runtime::Match->new({
        bool  => $bool,
        match => $1,
        tail  => $2
    });
}
sub varhash {
    my $grammar =  shift;
    $_[0] = "" unless defined $_[0];
    my $bool = $_[0] =~ / ^(\%(?:(?:\:\:)?[_[:alnum:]]+)+)(.*)$ /xs;
    return Pugs::Runtime::Match->new({
        bool  => $bool,
        match => $1,
        tail  => $2
    });
}
sub varglobal {
    my $grammar =  shift;
    $_[0] = "" unless defined $_[0];
    my $bool = $_[0] =~ / ^([\$|\@|\%]\*(?:(?:\:\:)?[_[:alnum:]]+)+)(.*)$ /xs;
    return Pugs::Runtime::Match->new({
        bool  => $bool,
        match => $1,
        tail  => $2
    });
}

1;
