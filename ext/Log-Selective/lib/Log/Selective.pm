module Log::Selective-0.01-BRENTDAX;

my $active;
for split ' ', %*ENV<SELECT_LOGS> -> $_ is copy {
	s:perl5:g{^:}{};
	$active |= $_;
}

sub select(*%newtags) {
	$active |= any(keys %newtags);
}

sub note(*@msg is copy, *%tags is copy) is export {
	my $caller=caller;
	
	unless %tags {
		my $package=$caller.package;
		$package ~~ s{.*\:\:}{};
		%tags{$package}=1;
	}
	
	@msg = ["Note: currently"] unless @msg;
	
	if $active eq any('all', keys %tags) {
		unless @msg[-1] ~~ /\n$/ {
			@msg.push(" at $caller.file() line $caller.line().\n");
		}
		print $ERR: @msg;
		return [~]@msg #but true;
	}
	return;
}

=head1 TITLE

Log::Selective - Selectively choose what to log

=head1 SYNOPSIS

	use Log::Selective;
	Log::Selective::select(:tagA, :main);
	note "I'm here", :tagA;			# prints "I'm here at somefile.pl line 3."
	note "Now I'm here", :tagB;		# doesn't print, tagB isn't active
	note "And here too!";			# Implicit tag 'main', is printed

=head1 DESCRIPTION

Log::Selective is a Perl 6 module for printing simple log messages to C<$ERR>. 
It allows sets of messages to be selected by use of I<tags>; messages whose 
tags haven't been selected won't be printed.  It is designed for so-called 
"print-based debugging", but can be put to other uses as well.

Log::Selective exports a single function, C<note>, which is called with zero or 
more strings, followed by zero or more tags.  The strings are printed to 
C<$ERR> only if one or more of the tags have been "selected", either by giving 
them to the unexported C<Log::Selective::select> function or by including them 
in the C<SELECT_LOGS> environment variable.  There is no facility to deselect a 
tag.

=head2 Tags

A tag is always represented by a named parameter (pair).  (The value of the 
pair is currently unused, but for forward compatibility it should always be the 
value C<1>, the default for the C<:name> form.)  By convention, tags are 
lowercase.

If C<note> is not provided with any tags, it will intuit one by extracting the 
last component of the calling function's package name.  (For example, if 
a function in C<Some::Module> called C<note> without providing any tags, it 
would look for a tag named C<Module>.)  Since module names are typically 
capitalized, most "autotags" will be too.

When activated, the special tag C<:all> will cause all tracing messages to be 
printed, even if none of their tags match.

=head1 BUGS

None known.  Report any to the author; patches are always appreciated.

=head1 COPYRIGHT

Copyright (C) 2005 Brent Royal-Gordon <brent@brentdax.com>. All Rights Reserved.

This program is Free Software, and may be used, distributed and modified under 
the same terms as Perl itself.

=cut
