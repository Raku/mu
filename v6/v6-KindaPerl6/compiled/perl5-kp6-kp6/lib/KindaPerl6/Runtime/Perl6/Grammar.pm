{

    package KindaPerl6::Grammar;

    # Do not edit this file - Perl 5 generated by KindaPerl6
    # AUTHORS, COPYRIGHT: Please look at the source file.
    use v5;
    use strict;
    no strict "vars";
    use constant KP6_DISABLE_INSECURE_CODE => 0;
    use KindaPerl6::Runtime::Perl5::Runtime;
    my $_MODIFIED;
    INIT { $_MODIFIED = {} }
    INIT {
        $_ =
          ::DISPATCH( $::Scalar, "new",
            { modified => $_MODIFIED, name => "$_" } );
    }
    do {
        do {
            if (
                ::DISPATCH(
                    ::DISPATCH(
                        ::DISPATCH(
                            (
                                $GLOBAL::Code_VAR_defined =
                                  $GLOBAL::Code_VAR_defined
                                  || ::DISPATCH( $::Routine, "new", )
                            ),
                            'APPLY',
                            $::KindaPerl6::Grammar
                        ),
                        "true"
                    ),
                    "p5landish"
                )
              )
            {
            }
            else {
                do {
                    do {
                        ::MODIFIED($::KindaPerl6::Grammar);
                        $::KindaPerl6::Grammar = ::DISPATCH(
                            ::DISPATCH(
                                $::Class, 'new',
                                ::DISPATCH(
                                    $::Str, 'new', 'KindaPerl6::Grammar'
                                )
                            ),
                            'PROTOTYPE',
                        );
                      }
                  }
            }
        };
        do {
            use vars qw($_rule_space);
            $_rule_space =
qr (?{ local $GLOBAL::_M = [ $GLOBAL::_M, 'create', pos(), \$_ ]; $GLOBAL::_M2 = $GLOBAL::_M; })[[:space:]](?{ local $GLOBAL::_M = [ $GLOBAL::_M, 'to', pos() ]; $GLOBAL::_M2 = $GLOBAL::_M; }) x;
            ::DISPATCH(
                ::DISPATCH( $::KindaPerl6::Grammar, "HOW" ),
                "add_method",
                ::DISPATCH( $::Str, "new", "space" ),
                ::DISPATCH(
                    $::Method,
                    "new",
                    {
                        code => sub {
                            local $GLOBAL::_Class = shift;
                            undef $GLOBAL::_M2;
                            ( ref($_) ? ::DISPATCH( $_, "Str" )->{_value} : $_ )
                              =~ /$_rule_space/;
                            if ( $GLOBAL::_M2->[1] eq 'to' ) {
                                Match::from_global_data($GLOBAL::_M2);
                                $MATCH = $GLOBAL::MATCH = pop @Match::Matches;
                            }
                            else { $MATCH = $GLOBAL::MATCH = Match->new(); }
                            @Match::Matches = ();
                            return $MATCH;
                          }
                    }
                ),
            );
        };
        do {
            use vars qw($_rule_word);
            $_rule_word =
qr (?{ local $GLOBAL::_M = [ $GLOBAL::_M, 'create', pos(), \$_ ]; $GLOBAL::_M2 = $GLOBAL::_M; })[[:word:]](?{ local $GLOBAL::_M = [ $GLOBAL::_M, 'to', pos() ]; $GLOBAL::_M2 = $GLOBAL::_M; }) x;
            ::DISPATCH(
                ::DISPATCH( $::KindaPerl6::Grammar, "HOW" ),
                "add_method",
                ::DISPATCH( $::Str, "new", "word" ),
                ::DISPATCH(
                    $::Method,
                    "new",
                    {
                        code => sub {
                            local $GLOBAL::_Class = shift;
                            undef $GLOBAL::_M2;
                            ( ref($_) ? ::DISPATCH( $_, "Str" )->{_value} : $_ )
                              =~ /$_rule_word/;
                            if ( $GLOBAL::_M2->[1] eq 'to' ) {
                                Match::from_global_data($GLOBAL::_M2);
                                $MATCH = $GLOBAL::MATCH = pop @Match::Matches;
                            }
                            else { $MATCH = $GLOBAL::MATCH = Match->new(); }
                            @Match::Matches = ();
                            return $MATCH;
                          }
                    }
                ),
            );
        };
        do {
            use vars qw($_rule_digit);
            $_rule_digit =
qr (?{ local $GLOBAL::_M = [ $GLOBAL::_M, 'create', pos(), \$_ ]; $GLOBAL::_M2 = $GLOBAL::_M; })[[:digit:]](?{ local $GLOBAL::_M = [ $GLOBAL::_M, 'to', pos() ]; $GLOBAL::_M2 = $GLOBAL::_M; }) x;
            ::DISPATCH(
                ::DISPATCH( $::KindaPerl6::Grammar, "HOW" ),
                "add_method",
                ::DISPATCH( $::Str, "new", "digit" ),
                ::DISPATCH(
                    $::Method,
                    "new",
                    {
                        code => sub {
                            local $GLOBAL::_Class = shift;
                            undef $GLOBAL::_M2;
                            ( ref($_) ? ::DISPATCH( $_, "Str" )->{_value} : $_ )
                              =~ /$_rule_digit/;
                            if ( $GLOBAL::_M2->[1] eq 'to' ) {
                                Match::from_global_data($GLOBAL::_M2);
                                $MATCH = $GLOBAL::MATCH = pop @Match::Matches;
                            }
                            else { $MATCH = $GLOBAL::MATCH = Match->new(); }
                            @Match::Matches = ();
                            return $MATCH;
                          }
                    }
                ),
            );
        };
        do {
            use vars qw($_rule_backslash);
            $_rule_backslash =
qr (?{ local $GLOBAL::_M = [ $GLOBAL::_M, 'create', pos(), \$_ ]; $GLOBAL::_M2 = $GLOBAL::_M; })\\(?{ local $GLOBAL::_M = [ $GLOBAL::_M, 'to', pos() ]; $GLOBAL::_M2 = $GLOBAL::_M; }) x;
            ::DISPATCH(
                ::DISPATCH( $::KindaPerl6::Grammar, "HOW" ),
                "add_method",
                ::DISPATCH( $::Str, "new", "backslash" ),
                ::DISPATCH(
                    $::Method,
                    "new",
                    {
                        code => sub {
                            local $GLOBAL::_Class = shift;
                            undef $GLOBAL::_M2;
                            ( ref($_) ? ::DISPATCH( $_, "Str" )->{_value} : $_ )
                              =~ /$_rule_backslash/;
                            if ( $GLOBAL::_M2->[1] eq 'to' ) {
                                Match::from_global_data($GLOBAL::_M2);
                                $MATCH = $GLOBAL::MATCH = pop @Match::Matches;
                            }
                            else { $MATCH = $GLOBAL::MATCH = Match->new(); }
                            @Match::Matches = ();
                            return $MATCH;
                          }
                    }
                ),
            );
        };
        do {
            use vars qw($_rule_newline);
            $_rule_newline =
qr (?{ local $GLOBAL::_M = [ $GLOBAL::_M, 'create', pos(), \$_ ]; $GLOBAL::_M2 = $GLOBAL::_M; })(?m)(\n\r?|\r\n?)(?{ local $GLOBAL::_M = [ $GLOBAL::_M, 'to', pos() ]; $GLOBAL::_M2 = $GLOBAL::_M; }) x;
            ::DISPATCH(
                ::DISPATCH( $::KindaPerl6::Grammar, "HOW" ),
                "add_method",
                ::DISPATCH( $::Str, "new", "newline" ),
                ::DISPATCH(
                    $::Method,
                    "new",
                    {
                        code => sub {
                            local $GLOBAL::_Class = shift;
                            undef $GLOBAL::_M2;
                            ( ref($_) ? ::DISPATCH( $_, "Str" )->{_value} : $_ )
                              =~ /$_rule_newline/;
                            if ( $GLOBAL::_M2->[1] eq 'to' ) {
                                Match::from_global_data($GLOBAL::_M2);
                                $MATCH = $GLOBAL::MATCH = pop @Match::Matches;
                            }
                            else { $MATCH = $GLOBAL::MATCH = Match->new(); }
                            @Match::Matches = ();
                            return $MATCH;
                          }
                    }
                ),
            );
        };
        do {
            use vars qw($_rule_not_newline);
            $_rule_not_newline =
qr (?{ local $GLOBAL::_M = [ $GLOBAL::_M, 'create', pos(), \$_ ]; $GLOBAL::_M2 = $GLOBAL::_M; }).(?{ local $GLOBAL::_M = [ $GLOBAL::_M, 'to', pos() ]; $GLOBAL::_M2 = $GLOBAL::_M; }) x;
            ::DISPATCH(
                ::DISPATCH( $::KindaPerl6::Grammar, "HOW" ),
                "add_method",
                ::DISPATCH( $::Str, "new", "not_newline" ),
                ::DISPATCH(
                    $::Method,
                    "new",
                    {
                        code => sub {
                            local $GLOBAL::_Class = shift;
                            undef $GLOBAL::_M2;
                            ( ref($_) ? ::DISPATCH( $_, "Str" )->{_value} : $_ )
                              =~ /$_rule_not_newline/;
                            if ( $GLOBAL::_M2->[1] eq 'to' ) {
                                Match::from_global_data($GLOBAL::_M2);
                                $MATCH = $GLOBAL::MATCH = pop @Match::Matches;
                            }
                            else { $MATCH = $GLOBAL::MATCH = Match->new(); }
                            @Match::Matches = ();
                            return $MATCH;
                          }
                    }
                ),
            );
          }
    };
    1
}
