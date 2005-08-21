#!pugs
use v6;

module t_LKT_B_L_Eng;

my Str $xy is constant = 'BE';
my Str %text_strings is constant = (
    'two' => $xy~' - sky pie rye',
    'three' => $xy~' - eat {knife}',
);

sub get_text_by_key( Str $msg_key ) returns Str {
    return %text_strings{$msg_key};
}
