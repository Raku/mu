package Smart::Links;
use strict;
use warnings;
use 5.006;

our $VERSION = '0.01';

use File::ShareDir;
use FindBin;
use base 'Class::Accessor';
__PACKAGE__->mk_accessors(qw(check line_anchor smoke_rev));

=head1 NAME

Smart::Links - connecting test files with pod documentation

=head1 SYNOPTIS

 smartlinks.pl
 
=head1 DESCRIPTION

=cut

sub new {
	my $class = shift;

	my $self = $class->SUPER::new(@_);

	$self->{link_count}        = 0;
	$self->{broken_link_count} = 0;

	return $self;
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

=begin private

=head2 add_link

  add_link($linktree, $synopsis, $section, $pattern, $infile, $from, $to);

=end private

=cut

# TODO add tests
sub add_link  {
    my ($self, $linktree, $synopsis, $section, $pattern, $t_file, $from, $to) = @_;
    if ($from == $to) {
        warn "WARNING: empty snippet detected at $t_file (line $from ~ $to).\n";
    }
    $linktree->{$synopsis} ||= {};
    $linktree->{$synopsis}->{$section} ||= [];
    if ($pattern and substr($pattern, -1, 1) eq '/') { $pattern = "/$pattern"; }
    push @{ $linktree->{$synopsis}->{$section} },
        [$pattern => [$t_file, $from, $to]];
        
    return $self->link_count_inc;
}

sub emit_pod {
    my ($self, $podtree) = @_;

    my $str;
    $str .= $podtree->{_header} if $podtree->{_header};
    for my $elem (@{ $podtree->{_sections} }) {
        my ($num, $sec) = @$elem;
        $str .= "=head$num $sec\n\n";
        for my $para (@{ $podtree->{$sec} }) {
            if ($para eq '') {
                $str .= "\n";
            } elsif ($para =~ /^\s+/) {
                $str .= $para;
            } else {
                $str .= "$para\n";
            }
        }
    }
    $str = "=pod\n\n_LINE_ANCHOR_1\n\n$str" if $self->line_anchor;

    return $str;
}

sub parse_pod {
    my ($self, $infile) = @_;
    open my $in, $infile or
        die "can't open $infile for reading: $!\n";
    my $podtree = {};
    my $section;
    while (<$in>) {
        if (/^ =head(\d+) \s* (.*\S) \s* $/x) {
            #warn "parse_pod: *$1*\n";
            my $num = $1;
            $section = $2;
            $podtree->{_sections} ||= [];
            push @{ $podtree->{_sections} }, [$num, $section];
        } elsif (!$section) {
            $podtree->{_header} .= $_;
        } elsif (/^\s*$/) {
            $podtree->{$section} ||= [];
            #push @{ $podtree->{$section} }, "\n";
            my @new = ('');;
            if ($self->line_anchor and $podtree->{$section}->[-1] !~ /^=over\b|^=item\b/) {
                unshift @new, "_LINE_ANCHOR_$.\n";
            }
            push @{ $podtree->{$section} }, @new;
        } elsif (/^\s+(.+)/) {
            $podtree->{$section} ||= [''];
            $podtree->{$section}->[-1] .= $_;
            push @{ $podtree->{$section} }, '';
        } else {
            $podtree->{$section} ||= [''];
            $podtree->{$section}->[-1] .= $_;
        }
    }
    close $in;
    $podtree;
}

sub process_t_file {
    my ($self, $infile, $linktree) = @_;
    open my $in, $infile or
        die "error: Can't open $infile for reading: $!\n";
    my ($setter, $from, $to);
    my $found_link = 0;
    while (<$in>) {
        chomp;
        my $new_from;
        my ($synopsis, $section, $pattern);
        if (/^ \s* \#? \s* L< (S\d+) \/ ([^\/]+) >\s*$/xo) {
            ($synopsis, $section) = ($1, $2);
            $section =~ s/^\s+|\s+$//g;
            $section =~ s/^"(.*)"$/$1/;
            #warn "$synopsis $section" if $synopsis eq 'S06';
            $new_from = $.;
            $to = $. - 1;
            $found_link++;
        }
        elsif (/^ \s* \#? \s* L(<<?) (S\d+) \/ ([^\/]+) \/ (.*) /xo) {
            #warn "$1, $2, $3\n";
            my $brackets;
            ($brackets, $synopsis, $section, $pattern) = ($1, $2, $3, $4);
            $brackets = length($brackets);
            $section =~ s/^\s+|\s+$//g;
            $section =~ s/^"(.*)"$/$1/;
            if (!$section) {
                $self->error("$infile: line $.: section name can't be empty.");
            }
            $pattern =~ s/^\s+|\s+$//g;
            if (substr($pattern, -1, 1) ne '>') {
                $_ = <$in>;
                s/^\s*\#?\s*|\s+$//g;
                if (!s/>{$brackets}$//) {
                    $self->error("$infile: line $.: smart links must terminate",
                        "in the second line.");
                    next;
                }
                $pattern .= " $_";
                $new_from = $. - 1;
                $to = $. - 2;
            } else {
                $new_from = $.;
                $to = $. - 1;
                $pattern =~ s/\s*>{$brackets}$//;
            }
            #warn "*$synopsis* *$section* *$pattern*\n";
            $found_link++;
        }
        elsif (/^ \s* \#? \s* L<? S\d+\b /xoi) {
            $self->error("$infile: line $.: syntax error in the magic link:\n\t$_");
        }
        else { next; }

        #warn "*$synopsis* *$section*\n";
        if ($from and $from == $to) {
            my $old_setter = $setter;
            my $old_from = $from;
            $setter = sub {
                $self->add_link($linktree, $synopsis, $section, $pattern, $infile, $_[0], $_[1]);
                $old_setter->($old_from, $_[1]);
                #warn "$infile - $old_from ~ $_[1]";
            };
            #warn "$infile - $from ~ $to";
        } else {
            $setter->($from, $to) if $setter and $from;
            $setter = sub {
                $self->add_link($linktree, $synopsis, $section, $pattern, $infile, $_[0], $_[1]);
            };
        }
        $from = $new_from;
    }
    $setter->($from, $.) if $setter and $from;
    close $in;
#   print "No smartlink found in <$infile>\n" if (defined $print_missing && $found_link == 0);
    return $found_link;
}

sub process_yml_file {
	my ($self, $yml_file) = @_;
    if ($yml_file) {
        eval {
            require Test::TAP::Model;
            require YAML::Syck;
        };
        if ($@) {
            die "--smoke-res option requires both Test::TAP::Model and YAML::Syck. ".
                "At least one of them is not installed.\n";
        }
        my $data = YAML::Syck::LoadFile($yml_file);
        #warn $data;
        my $structure;
        if ($data->{meat}) {
            $structure = delete $data->{meat};
        }
        my $tap = Test::TAP::Model->new_with_struct($structure);
        for my $file ($tap->test_files) {
            #warn "  $file...\n";
            (my $fname = $file->name) =~ s{.*?/t/}{t/};
            my %file_info;
            $self->{test_result}->{$fname} = \%file_info;
            for my $case ($file->cases) {
                next if $case->skipped or !$case->test_line;
                $file_info{$case->test_line} = $case->actual_ok;
            }
        }
        #YAML::Syck::DumpFile('test_result.yml', $self->{test_result});
        my $smoke_rev = $data->{revision};
        $self->smoke_rev($smoke_rev);
        $smoke_rev = $smoke_rev ? "r$smoke_rev" : 'unknown';
        warn "info: pugs smoke is at $smoke_rev.\n";
    }
}



sub link_count_inc { $_[0]->{link_count}++ };
sub link_count     { $_[0]->{link_count} };

sub broken_link_count_inc { $_[0]->{broken_link_count}++ };
sub broken_link_count     { $_[0]->{broken_link_count} };

sub error {
	my $self = shift;
    if ($self->check) { warn "ERROR: @_\n"; }
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
