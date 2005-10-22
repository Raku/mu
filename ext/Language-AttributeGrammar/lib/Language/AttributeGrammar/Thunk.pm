class Language::AttributeGrammar::Thunk;

has $.value;
has $.stage;

submethod BUILD() {
    $.stage = 1;
}

method set($dep, $code) {
    die "Attempt to set a stage>1 thunk" if $.stage > 1;
    $.value = { 
        dep => $dep,
        code => $code,
    };
    $.stage++;
}

method get() {
    die "Attempt to evaluate a stage 1 thunk" if $.stage == 1;
    die "Infinite loop" if $.stage == 3;
    
    given $.stage {
        when 1  { die "Attempt to evaluate a stage 1 thunk" }
        when 2  { 
            $.stage++;
            $.value = $.value<code>.($.value<dep>);
            $.stage++;
            $.value;
        }
        when 3  { die "Infinite loop" }
        when 4  { $.value }
        default { die "XXX Huh?  How did we get to stage $.stage?" }
    }
}

# vim: ft=perl6 :
