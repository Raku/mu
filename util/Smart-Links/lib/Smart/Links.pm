package Smart::Links;
use strict;
use warnings;
use 5.006;

our $VERSION = '0.01';

use File::ShareDir;
use FindBin;

=head1 NAME

Smart::Links - connecting test files with pod documentation

=head1 SYNOPTIS

 smartlinks.pl
 
=head1 DESCRIPTION

=cut

sub new {
	return bless {}, shift;
}


# convert patterns used in 00-smartlinks.to perl 5 regexes
sub parse_pattern {
	my ($self, $pat) = @_;

    my @keys;
    while (1) {
        if ($pat =~ /\G\s*"([^"]+)"/gc ||
            $pat =~ /\G\s*'([^']+)'/gc ||
            $pat =~ /\G\s*(\S+)/gc) {
                push @keys, $1;
        } else { last }
    }
    my $str = join('.+?', map {
        my $key = quotemeta $_;
        $key =~ s/^\w/\\b$&/;
        $key =~ s/\w$/$&\\b/;
        $key;
    } @keys);

    $str;
}

# process paragraphs of the synopses: unwrap lines, strip POD tags, and etc.
sub process_paragraph {
	my ($self, $str) = @_;

    # unwrap lines:
    $str =~ s/\s*\n\s*/ /g;

    # strip POD tags:
    # FIXME: obviously we need a better way to do this:
    $str =~ s/[LCFIB]<<<\s+(.*?)\s+>>>/$1/g;
    $str =~ s/[LCFIB]<<\s+(.*?)\s+>>/$1/g;
    $str =~ s/[LCFIB]<(.*?)>/$1/g;
    $str;
}

sub get_javascript {

	# for the test scripts in t/ and the smartlinks.pl in script/  directory
	my $file = File::Spec->catfile($FindBin::Bin, '..', 'share', 'smartlinks.js');
	
	if (not -e $file) {
		# for smarlinks.pl in utils/ directory of Pugs if Smart::Links is not installed
		$file = File::Spec->catfile($FindBin::Bin, 'Smart-Links', 'share', 'smartlinks.js');
	}

	# installed version of the file
	if (not -e $file) {
		$file = File::Spec->catfile(File::ShareDir::dist_dir('Smart::Links'), 'smartlinks.js');
	}
	if (not $file) {
		warn "Could not find 'smartlinks.js'\n";
		return '';
	}
	#warn $file;
	if (open my $fh, '<', $file) {
		local $/ = undef;
		return <$fh>;
	}
	warn "could not open '$file'";
	return '';
}

=head1 AUTHOR

Agent Zhang (E<lt>agentzh@gmail.comE<gt>) wrote the initial
implementation, getting help from many others in the Pugs team.

Current maintainer: The Pugs team

=head1 COPYRIGHT

Copyright (c) 2006 - 2009 by the Pugs Team.

=head1 LICENSE

Smart::Links is free software; you can redistribute it and/or modify it under the
terms of the Artistic License 2.0.  (Note that, unlike the Artistic License
1.0, version 2.0 is GPL compatible by itself, hence there is no benefit to
having an Artistic 2.0 / GPL disjunction.)

=cut


1;
