=pod
XXX I suspect the where{true}'s are not the right thing.  What is?

The definition of these in terms of smartmatch seem like the kind of
thing which an implementation will overwrite. So smartmatch can be
implemented using some is_subtype? (QQQ what's it called?) test.  But
maybe not.  Either way, the current approach seems plausible, no?

Clutter::int_sizes 1,2,4... doesnt add much clarity, given the cruft
of wrapping a map and join around the macro string.  But perhaps there
should be a Helpers::join_map ns, { "...$^n..." }, which might tilt
the balance.

Clutter:: is a package for macros declared and used locally.
Helpers:: is for more generally useful stuff, and need not be
  implemented immediately.  Assume the existence of what would
  be useful, and we'll do it later.

=cut
###*** Value types - numeric

macro Clutter::int_size($n) {
  'subset  int'~$n~' of ValueType where { $^n ~~ Int and -2**'~$n~' <= $^n <  2**'~$n~' };
   subset uint'~$n~' of ValueType where { $^n ~~ Int and      0     <= $^n <= 2**'~$n~' };'
}
Clutter::int_size 1;
Clutter::int_size 2;
Clutter::int_size 4;
Clutter::int_size 8;
Clutter::int_size 16;
Clutter::int_size 32;
Helpers::macif(%?CONFIG{'has_int64'},'Clutter::int_size 64;');

#XXX- rename $n or $^n.  having the two of them "n" is confusing.
macro Clutter::num_size($n) {
  'subset num'~$n~'     of ValueType where { $^n ~~ Num and ...XXXfill in... };
   subset complex'~$n~' of ValueType where { $^n ~~ Num and ...XXXfill in... };'
}
Clutter::num_size 32;
Clutter::num_size 64;
Helpers::macif(%?CONFIG{'has_num128'},'Clutter::num_size 128;');


subset bit  of uint1 where { true };
subset byte of uint8 where { true };

Helpers::macif(%?CONFIG{'int_width'} == 32,'subset int of int32 where { true }');
Helpers::macif(%?CONFIG{'int_width'} == 64,'subset int of int64 where { true }');

Helpers::macif(%?CONFIG{'num_width'} == 32, 'subset num of num32  where { true }');
Helpers::macif(%?CONFIG{'num_width'} == 64, 'subset num of num64  where { true }');
Helpers::macif(%?CONFIG{'num_width'} == 128,'subset num of num128 where { true }');

Helpers::macif(%?CONFIG{'num_width'} == 32, 'subset complex of complex32  where { true }');
Helpers::macif(%?CONFIG{'num_width'} == 64, 'subset complex of complex64  where { true }');
Helpers::macif(%?CONFIG{'num_width'} == 128,'subset complex of complex128 where { true }');

