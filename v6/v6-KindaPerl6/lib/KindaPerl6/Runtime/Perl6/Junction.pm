# Junction.pm

# ChangeLog
#
# 2007-08-29
# * moved to kp6
# 2005-09-27
# * created this file (PIL-Run)
#

class Junction {
    has $.type;
    has $.things;

    method str { 
                my %sep = {
                    "any" =>" | ",
                    "none"=>" , ",
                    "all" =>" & ",
                    "one" =>" ^ ",
                };  
                
                      ( ($.type eq '!') ?? 'none' !! '' )
                    ~ "( "  
                    ~ ( @( $.things ) ).join( %sep{ $.type } )
                    ~ " )"  
     };

    method true {
        my $thing;  # XXX
        if $.type eq 'any' {
            for @( $.things ) -> $thing {
                if $thing { return True; };
            };
            return False;
        };
        if $.type eq 'all' {
            for @( $.things ) -> $thing {
                if !$thing { return False; };
            };
            return True;
        };
        if $.type eq 'none' {
            for @( $.things ) -> $thing {
                if $thing { return False; };
            };
            return True;
        };
        if $.type eq 'one' {
            my $counter = 0;
            for @( $.things ) -> $thing {
                if $thing { 
                    ++$counter;
                    if $counter > 1 {
                        return False;
                    };
                };
            };
            return $counter == 1;
        };
    };

};

# vim: sw=4 ts=4 expandtab syn=perl6
