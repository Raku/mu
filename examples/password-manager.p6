#!/usr/bin/pugs

=begin pod

=NAME Perl6 Password Manager
=AUTHOR Ryan "rhr" Richter <ryan@tau.solarneutrino.net>

=for DESCRIPTION
This program will generate, store, and retrieve passwords.
It uses B<xclip> to transfer passwords through the X11 clipboard,
so in normal use you will never even need to see your (unique,
randomly generated) passwords.  Although it uses a terminal
L<ReadLine|perl5:Term::ReadLine> interface, it is designed
to be used mostly with the mouse, via cut-n-paste.
It can even generate random Unicode passwords!

=begin SYNOPSIS
=begin input
T<<> >>.h							I<# display usage information>
T<<> >>.n	R<account-name>		R	R<username>	I<# add a new account with a random password>
T<<> >>/R<account>						I<# search database for matching account names>
T<<> >>R<account>						I<# xclip password for R<account> and exit>
=end input
This last command ignores leading and trailing whitespace,
so that you can sloppily select the account name from the
output of the C</> command.
=end SYNOPSIS

=begin USAGE
		<account>	xclip account password and exit
		/<regex>	search accounts
		.n		new account
		.d		delete account
		.p		print account password
		.x		xclip account password
		.c		commit changes
		.r		xclip random password
		.R		print random password
		.a		switch to alphanum
		.A		switch to all printable
		.u		switch to unicode
		.U		switch to ASCII
		.l		change random password length
		.h		help
=end USAGE

=end pod

use perl5:Term::ReadLine;

regex alphanum	{ ^ <!before \t><alnum> $ }
regex printable	{ ^ <!before \t><print> $ }
my Regex $pwchar := &alphanum;
my Range $ascii = 0..127;
my Range $unicode = 0..0x10ffff;
my Range $charset := $ascii;
my Int $len = 8;
my Bool $changed = False;
my Hash of Str %pw;

sub help(--> Void) { warn $=USAGE; }

my Code &abort := -> Str $err { warn "$err\n"; return; }

sub search(Str $pat --> Void) {
	for %pw.keys -> $k { say %pw{$k}<user>, "\t", $k if $k ~~ /<$pat>/ }
}

sub mk(Str $acct, Str $pass is copy, Str $user --> Void) {
	$changed = True;
	$pass = randpass if $pass eq 'R';
	%pw{$acct}<pass user> = $pass, $user;
}

sub del(Str $acct --> Void) {
	abort "No account $acct" unless %pw{$acct}.:exists;
	$changed = True;
	%pw{$acct}.:delete;
}

sub pr(Str $acct --> Void) {
	abort "No account $acct" unless %pw{$acct}.:exists;
	say %pw{$acct}<user pass>.join("\t");
}

sub wxclip(Str $acct --> Void) {
	abort "No account $acct" unless %pw{$acct}.:exists;
	xclip %pw{$acct}<pass>;
}

sub xclip(Str $s --> Void) {
	my IO $xclip = Pipe.to: 'xclip' orelse abort 'No xclip - use .p';
	$xclip.print: $s;
	$xclip.close;
}

sub sx(Str $s --> Void) {
	my Str $pw = %pw{$s}<pass> //
		first Str, (%pw{$_}<pass> if /$s/ for %pw.keys)
		orelse abort "Couldn't find account $s";
	xclip $pw;
	cmt if $changed;
	sleep 10;
	xclip '';
	exit;
}

sub randpass(--> Str) {
	my Str $c;
	# < TimToady> and 9 developers out of 10 will shoot you if you use that construct. :)
	# < TimToady> at least, if you use it uncommented...
	my Str @password := gather while @password < $len {
		if ($c = $charset.pick.chr) ~~ $pwchar { take $c }
	}
	return [~] @password;
}

sub cmt(--> Void) {
	unlink 'pwd.gpg.old' orelse abort "Couldn't unlink: $!";
	rename 'pwd.gpg', 'pwd.gpg.old' orelse abort "Couldn't rename: $!";
	my IO $pwd = Pipe.to: 'gpg --symmetric --force-mdc --cipher-algo AES256 --output pwd.gpg'
		orelse abort "Couldn't encrypt: $!";
	for %pw.keys -> $k { $pwd.say: $k, "\t", %pw{$k}<pass user>.join("\t") }
	if $pwd.close {
		$changed = False;
	} else {
		abort "Couldn't write pwd: $!";
	}
}

regex cmd {
	^^
	[ '/' $<pat> = [ \N* ] { search $<pat> }
	| \s* <!before '.'> $<acct> = [ \T+? ] \s* $$ { sx $<acct> }
	| '.'	[ n	[ \t $<acct> = [ \T+ ] \t $<pass> = [ \T+ ] \t $<user> = [ \T+ ] $$
				{ mk $<acct>, $<pass>, $<user> }
			| { warn ".n [tab] account [tab] password [tab] username\n" } <commit> <fail>
			]
		| d	[ \s+ $<acct> = [ \T+? ] \s* $$ { del $<acct> }
			| { warn ".d account\n" } <commit> <fail>
			]
		| p	[ \s+ $<acct> = [ \T+? ] \s* $$ { pr $<acct> }
			| { warn ".p account\n" } <commit> <fail>
			]
		| x	[ \s+ $<acct> = [ \T+? ] \s* $$ { wxclip $<acct> }
			| { warn ".x account\n" } <commit> <fail>
			]
		| l	[ \s+ $<len> = [ \d+ ] \s* $$ { $len = $<len> }
			| { warn ".l length\nlength is $len\n" } <commit> <fail>
			]
		| c { cmt }
		| r { xclip randpass }
		| R { say randpass }
		| a { $pwchar := &alphanum }
		| A { $pwchar := &printable }
		| u { $charset := $unicode }
		| u { $charset := $ascii }
		| h { help }
		| { warn "Bad command\n"; help; } <commit> <fail>
		]
	]
}

regex pwent {
	^^ $<acct> = [ \T+ ] \t $<pass> = [ \T+ ] \t $<user> = [ \T+ ] $$
}

%*ENV<PATH> = '/bin:/usr/bin:/usr/bin/X11';
umask 0o77;
chdir "$+HOME/pw" orelse die "Couldn't cd: $!";
my IO $pwd = Pipe.from: 'gpg --output - --decrypt pwd.gpg' orelse die "Couldn't decrypt: $!";
for =$pwd {
	/<pwent>/ or die 'Malformed line ', $pwd.linenum, ": $_\n";
	%pw{$<pwent><acct>}<pass user> = $<pwent><pass user>;
}
$pwd.close;

my $term = new Term::ReadLine: 'pw';
my $attribs = $term.Attribs;
$attribs<completion_entry_function> = $attribs<list_completion_function>;
$attribs<completion_word> = %pw.keys;

while defined $_ = $term.readline('> ')  {
	/<cmd>/;
	NEXT { $attribs<completion_word> = %pw.keys; }
}
cmt if $changed;
