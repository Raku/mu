use MooseX::Declare;
class VAST::sublongname {
    use utf8;
    use AST::Helpers;
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
            my $ret = $self->{sigil}{TEXT}.($self->{twigil}[0]{TEXT} || '').$shortname->{category}{TEXT}.':'.$shortname->{colonpair}[0]{v}{nibble}->Str . $single_variant;
            return $ret;
        }
    }
}
1;
