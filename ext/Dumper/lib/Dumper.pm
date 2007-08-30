=begin pod

=head1 NAME

Dumper - Simple dumper for Perl 6 structures.

=head1 SYNOPSIS

    use Dumper;

    my %struct = (
        foo => 1,
        bar => 'baz',
    );

    dump(\%struct);

    ---

    $DUMP6 = {
      foo = Int 1
      bar = Str baz
    }

=head1 DESCRIPTION

This is just a simple dumper for Perl 6 structures. Feel free to expand
it for your usage. Improvements are welcome every time.

=head1 SUBROUTINES

=head2 dump()

Call C<dump()> to dump a perl structure.

=head1 EXPORTS

Subroutine dump() is exportet.

=head1 BUGS

Please report all bugs to <jschulz.cpan(at)bloonix.de>.

=head1 AUTHOR

Perl 6 developers.

=head1 COPYRIGHT

This program is free software; you can redistribute it and/or modify it under the same terms as Perl itself.

=end pod

module Dumper-0001;
use v6-alpha;

sub dump is export(:ALL) {
    for (@_) {
        my $what = _dumper(0, '$DUMP6', $_);
        if ~$what {
            say "\$DUMP6 = $what $_";
        }
    }   
}

sub _dumper ($s, $x, $y) {
    my $what = $y.WHAT;

    if Array ~~ $what {
        say " " x $s, $x, " = [";
        _dump_array($s, $y);
        say " " x $s, "]";
    } elsif Hash ~~ $what {
        say " " x $s, $x, " = \{";
        _dump_hash($s, $y);
        say " " x $s, "}";
    } else {
        return $what;
    }

    return 0;
}

sub _dump_hash ($s is copy, %h) {
    $s += 2;
    for %h.kv -> $k, $v {
        my $what = _dumper($s, $k, $v);
        if ~$what {
            say " " x $s, "$k = $what $v";
        }
    }
}

sub _dump_array ($s is copy, @a) {
    $s += 2;
    my $i = 0;
    for @a -> $v {
        my $what = _dumper($s, $i, $v);
        if ~$what {
            say " " x $s, "$i = $what $v";
        }
        $i++;
    }
}

1;
