
package Date::Format::ISO8601;

# RULES GURUS APPLY HERE
our $iso8601_re is export = rx:perl5:ix/\s*
        # year
	(?: (\d{2}|\d{4}) -? | -)
        # month and day
        (?: (?: (\d\d) -? | - ) (\d\d) | (\d{3}) | [wW](\d{2}) )
        # optional time delimeter
	T? \s*
	# time - optional, defaults to 0:00
        (\d\d):?(\d\d):?(\d\d(?:\.\d*)?) \s*
	# time zone - optional, defaults to local time
	( Z | [+-](\d\d):?(\d\d)? )?
	/;      #/x  # (go cperl-mode ;))

our $iso8601_dur_re = rx:perl5:ix/\s*
        P \s*
	(?: (\d+) \s* Y )? \s*
	(?: (\d+) \s* M )? \s*
	(?: (\d+) \s* D )? \s*
        (?: T \s*
	    (?: (\d+) \s* H )? \s*
	    (?: (\d+) \s* M )? \s*
	    (?: (\d+) (?: \.(\d+))? \s* S )? \s* )?
	/;      #/x  # (go cperl-mode ;))

our $iso8601_duration_re is export = rx:perl5:ix/\s*
       (?:  ( # $0
              $iso8601_dur_re ( # $8
                                \057 $iso8601_re )? )
       |    ( # $18
              $iso8601_re \057 (?: ( # $28
                                 $iso8601_dur_re ) |
                                 ( # $36
				 $iso8601_re ) ) )
	)/;      #/x?# yay, cperl-mode

#FIXME - can't use

