use MooseX::Declare;
class VAST::longname {
    use utf8;
    use AST::Helpers;
    method canonical {
        my $single_variant = '';
        if ($self->{colonpair}[1]) {
           if ($self->{colonpair}[1]{signature}) {
               # TODO handle whitespace sensibly
               $single_variant = ':(' . $self->{colonpair}[1]{signature}->{MATCH}->Str . ')';
           } else {
               XXX;
           } 
        }
        my $name = $self->{name}{identifier}{TEXT};
        my $v = $self->{colonpair}[0]{v};
        if ($v) {
            $name . ':' . $v->{circumfix}{nibble}->Str . $single_variant;
        } else {
            $name;
        }
    }
    method components {
        my $nibbles = $self->{colonpair}[0]{v}{nibble}{nibbles}[0];
        my @components = ($self->{name}{identifier}{TEXT},map {$_->{identifier}[0]{TEXT}} @{$self->{name}{morename}});
        $components[-1] .= ':' . $nibbles if $nibbles;
        return @components;
    }
}
1;
