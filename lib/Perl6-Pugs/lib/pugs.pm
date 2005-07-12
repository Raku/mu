package pugs;
use strict;
use Filter::Simple;

$pugs::VERSION = '0.02';

FILTER {
    my $marker = 'XXXXXXXX';
    $marker++ while /^$marker$/m;
    $_ = <<END;
use Inline Pugs => <<'$marker';
$_
$marker
END
};

1;

