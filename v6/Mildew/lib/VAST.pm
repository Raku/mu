use v5.10;
use Carp;
use MooseX::Declare;
{
    package VAST;
    use Module::Pluggable sub_name=>"subclasses",search_path=>["VAST"],require=>1;
    # load all the VAST::* subclasses
    VAST->subclasses;
}
class VAST::Base {
    method Str {
        my $b = $self->{BEG};
        my $e = $self->{END};
        return '' if $b > length($::ORIG);
        substr($::ORIG, $b, $e - $b);
    }

    method emit_m0ld {
        use Mildew::AST::Helpers;
        if ($self->{infix}) {
            my $name = $self->{infix}{SYM};
            return fcall('&infix:'.$name,[map {$_->emit_m0ld} @{$self->{args}}]);
        }
        my @keys = grep {/^[a-z]\w*$/} keys %{$self};
        if (@keys == 1) {
#            say "falling back at ",ref $self," only key ",$keys[0];
            $self->{$keys[0]}->emit_m0ld;
        } else {
            #XXX improve error message
            die (ref $self," is not a simple node, possible choices are: ",join(' ',@keys));
        }
    }
}
1;
