use MooseX::Declare;
class VAST::sublongname {
    use utf8;
    use Mildew::AST::Helpers;
    method components {
        my $shortname = $self->{subshortname};
        if ($shortname->{desigilname}) {
            $shortname->{desigilname}{longname}->components;
        } elsif ($shortname->{category}) {
            my $single_variant = '';
            if ($shortname->{colonpair}[1]) {
               if ($shortname->{colonpair}[1]{fakesignature}) {
                   # TODO handle whitespace sensibly
                   $single_variant = ':(' . $shortname->{colonpair}[1]{fakesignature}->Str . ')';
               } else {
                   XXX;
               } 
            }
            # TODO find the source of the warning
            my $ret =
                ($self->{sigil}{TEXT} // '')
                . ($self->{twigil}[0]{TEXT} // '')
                . ($shortname->{category}{TEXT} // '')
                . ':'
                . $shortname->{colonpair}[0]{v}{circumfix}{nibble}->Str
                . $single_variant;
            return $ret;
        }
    }
}
1;
