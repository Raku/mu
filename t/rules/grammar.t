###!/usr/bin/pugs

use v6;
use Test;

plan 2;

=pod

Use rules from a grammar.

=cut

if(!eval('("a" ~~ /a/)')) {
  skip_rest "skipped tests - rules support appears to be missing";
  exit;
}

grammar yapc_schedule {
    rule ws { <any> }
    token speaker { \w[\w|\s]+ }
    rule talk { <'<small>'> <speaker> <'</small>'> }
    rule schedule { <talk>+ }
}

my $content = q:to/END/
    <tr>
    <td align="center" width="8%">8:30</td>
	<td align="center" width="23%"><a href="http://www.yapcchicago.org/the-schedule/monday/m-abstracts#am830">Conferences for Beginners</a><br><small>Jim Brandt, brian d foy</small></td>
	<td colspan="3" align="center" bgcolor="#3f3f3f" width="69%">—</td>
    </tr>
	<tr>
    <td align="center" width="8%">9:00</td>
	<td align="center" width="23%"><a href="http://www.yapcchicago.org/the-schedule/monday/m-abstracts#am900">Opening Ceremonies</a><br><small>Josh McAdams</small></td>
	<td colspan="3" align="center" bgcolor="#3f3f3f" width="69%">—</td>
    </tr>
END;

is($content ~~ m/<yapc_schedule.speaker>/, 'tr', 'read token from grammar namespace');
is($content ~~ m/<yapc_schedule.talk>/, '<small>Jim Brandt, brian d foy</small>', 'read rule from grammar namespace');
