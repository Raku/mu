package Regexp::Parser;

my $ENUM;
BEGIN { $ENUM = 0 }

use constant RPe_ZQUANT => --$ENUM, 'Quantifier unexpected on zero-length expression';
use constant RPe_NOTIMP => --$ENUM, 'Sequence (?%.*s...) not implemented';
use constant RPe_NOTERM => --$ENUM, 'Sequence (?#... not terminated';
use constant RPe_LOGDEP => --$ENUM, '(?p{}) is deprecated -- use (??{})';
use constant RPe_NOTBAL => --$ENUM, 'Sequence (?{...}) not terminated or not {}-balanced';
use constant RPe_SWNREC => --$ENUM, 'Switch condition not recognized';
use constant RPe_SWBRAN => --$ENUM, 'Switch (?(condition)... contains too many branches';
use constant RPe_SWUNKN => --$ENUM, 'Unknown switch condition (?(%.2s';
use constant RPe_SEQINC => --$ENUM, 'Sequence (? incomplete';
use constant RPe_BADFLG => --$ENUM, 'Useless (?%s%s) -- %suse /%s modifier';
use constant RPe_NOTREC => --$ENUM, 'Sequence (?%.*s...) not recognized';
use constant RPe_LPAREN => --$ENUM, 'Unmatched (';
use constant RPe_RPAREN => --$ENUM, 'Unmatched )';
use constant RPe_BCURLY => --$ENUM, 'Can\'t do {n,m} with n > m';
use constant RPe_NULNUL => --$ENUM, '%s matches null string many times';
use constant RPe_NESTED => --$ENUM, 'Nested quantifiers';
use constant RPe_LBRACK => --$ENUM, 'Unmatched [';
use constant RPe_EQUANT => --$ENUM, 'Quantifier follows nothing';
use constant RPe_BRACES => --$ENUM, 'Missing braces on \%s{}';
use constant RPe_RBRACE => --$ENUM, 'Missing right brace on \%s{}';
use constant RPe_BGROUP => --$ENUM, 'Reference to nonexistent group';
use constant RPe_ESLASH => --$ENUM, 'Trailing \\';
use constant RPe_BADESC => --$ENUM, 'Unrecognized escape \\%s%s passed through';
use constant RPe_BADPOS => --$ENUM, 'POSIX class [:%s:] unknown';
use constant RPe_OUTPOS => --$ENUM, 'POSIX syntax [%s %s] belongs inside character classes';
use constant RPe_EMPTYB => --$ENUM, 'Empty \%s{}';
use constant RPe_FRANGE => --$ENUM, 'False [] range "%s-%s"';
use constant RPe_IRANGE => --$ENUM, 'Invalid [] range "%s-%s"';

1;
