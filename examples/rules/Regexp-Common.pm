grammar Regexp::Common::ws {
    regex crop { [ ^ \s+ ] | [ \s+ $ ] }
}

grammar Regexp::Common::lingua {
    regex palindrome (Regex :$chars = /<alpha>/) {
        <$chars> | [ <$chars> <~~>? $0 ]
            # XXX not quite certain this is right :-)
    }
}

grammar Regexp::Common::list {
    regex list (Regex :$pat = /.*? \S/, Str|Regex :$sep = /\s* ',' \s*/, Str|Regex :$lsep = $sep) {
        [
            [ <$pet> <$sep> ]*
            <$pat> <$lsep> <$pat>
        ]
    }

    regex conj (Regex :$pat, Str|Regex :$sep, Regex :$word = /and | or/) {
        <list(:pat($pat) :sep($sep) :lsep(/\s* ','? \s* <$word> \s*/))>
    }
    
    regex and (Regex :$pat, Str|Regex :$sep) {
        <conj(:pat($pat) :sep($sep) :word(/and/))>
    }
    
    regex or (Regex :$pat, Str|Regex :$sep) {
        <conj(:pat($pat) :sep($sep) :word(/or/))>
    }
}