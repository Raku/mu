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
                    "none"=>" ! ",
                    "all" =>" & ",
                    "one" =>" ^ ",
                };  
                      "( "  
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
    };

};


