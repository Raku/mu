module WTemplate-0.0.1;
use v6;

## set up the globals

my %WIDGETS;

## set up the rules

rule sp {
    <[ ]>
}

rule id {
    <[a..z]><[a..z0..9]>+
}

rule elem {
    \< server \: (<id>) [<sp>+ (<id>) <sp>*=<sp> *"(<-["]>*)"]* <sp>* \> (.*?) \<\/ server \: $0 \>
}

rule block {
    \< server \: (<id>) [<sp>+ (<id>) <sp>*=<sp> *"(<-["]>*)"]* <sp>* \/\>
}

## the main subs

sub run_widget($match_in, %variables) {
    my $match = $match_in<elem> ?? $match_in<elem> !! $match_in<block>;
    my $widget = $match[0];
    my %parameters;
    %parameters{'_content'} = $match[3] if $match[3];
    if defined $match[1] and $match[1] ne '' {
        for 0..$match[1].elems-1 {
            %parameters{$match[1][$_]} = $match[2][$_];
        }
    }
    return %WIDGETS{$widget}(%parameters, %variables);
}

sub fill_with($template: %variables?) is export {
    my $return = $template;
    $return ~~ s:g! <elem> | <block> !{ run_widget($/, %variables) }!;
    return $return;
}

## some basic widgets

sub new_widget($name, &sub) is export {
    %WIDGETS{$name} = &sub;
}

new_widget 'text', sub (%p, %v) {
    return %v{%p{'id'}};
};

new_widget 'input', sub (%p, %v) {
    return '<input name="' ~ %p{'id'} ~ '" value="' ~ %v{%p{'id'}} ~ '" />';
};

new_widget 'hidden', sub(%p, %v) {
    return '<input type="hidden" name="' ~ %p{'id'} ~ '" value="' ~ %v{%p{'id'}} ~ '" />';
};

new_widget 'textarea', sub(%p, %v) {
    return '<textarea name="' ~ %p{'id'} ~ '">' ~ %v{%p{'id'}} ~ '</textarea>';
};

new_widget 'repeater', sub(%p, %v) {
    my $return;
    for %v{%p{'id'}} {
        $return ~= %p{'_content'}.fill_with(%$_);
    }
    return $return;
};
