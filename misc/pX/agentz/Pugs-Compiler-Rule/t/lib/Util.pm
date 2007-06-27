package t::lib::Util;

use Spiffy -Base;

our @EXPORT = qw( parse_str_list );

sub parse_str_list ($) {
    local $_ = shift;
    my @token;
    while (1) {
        /\G \s+ /gcmsx;
        if (/\G ' ( \\. | [^\\'] )*? ' /gcmsx) {
            push @token, eval $&;
            if ($@) { die "ERROR: $t::BloackName - invalid single-quotes: $&\n"; }
        }
        elsif (/\G " ( \\. | [^\\'] )*? " /gcmsx) {
            push @token, eval $&;
            if ($@) { die "ERROR: $t::BloackName - invalid double-quotes: $&\n"; }
        }
        elsif (/\G\S+/gcms) {
            push @token, $&;
        } else {
            last;
        }
    }
    wantarray ? @token : $token[0];
}

1;

