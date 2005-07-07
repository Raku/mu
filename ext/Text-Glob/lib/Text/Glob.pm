module Text::Glob-6.0.0;

use v6;

has Int $.debug;
has Int $.strict_leading_dot;
has Int $.strict_wildcard_slash;

submethod BUILD {
    $.debug                 = 0;
    $.strict_leading_dot    = 1;
    $.strict_wildcard_slash = 1;
}

method glob_to_regex ($glob) {
    my Str $regex      = '';
    my Int $in_curlies = 0;
    my Int $escaping   = 0;
    my Int $first_byte = 1;
    for $glob.split('') -> $char {
        if $first_byte {
            if $.strict_leading_dot {
                $regex ~= '(?=[^\.])' unless $char eq '.'
            }
            $first_byte = 0;
        }
        if ( $char eq '/' ) { $first_byte = 1 }
        if ( $char eq '.' || $char eq '(' || $char eq ')' || $char eq '|' ||
             $char eq '+' || $char eq '^' || $char eq '$' ) {
            $regex ~= "\\$char";
        }
        elsif ( $char eq '*' ) {
            $regex ~= $escaping ?? "\\*" ::
              $.strict_wildcard_slash ?? "[^/]*" :: ".*";
        }
        elsif ( $char eq '?' ) {
            $regex ~= $escaping ?? "\\?" ::
              $.strict_wildcard_slash ?? "[^/]" :: ".";
        }
        elsif ( $char eq '{' ) {
            $regex ~= $escaping ?? '\{' :: '(';
            ++$in_curlies unless $escaping;
        }
        elsif ( $char eq '}' && $in_curlies ) {
            $regex ~= $escaping ?? "}" :: ")";
            --$in_curlies unless $escaping;
        }
        elsif ( $char eq ',' && $in_curlies ) {
            $regex ~= $escaping ?? "," :: "|"
        }
        elsif ( $char eq "\\" ) {
            if ($escaping) {
                $regex ~= "\\\\";
                $escaping = 0;
            }
            else { $escaping = 1 }
            next;
        }
        else {
            $regex ~= $char;
            $escaping = 0;
        }
        $escaping = 0;
    }
    say "Regex is $regex" if $.debug;
    return m:P5/$regex/;
}

=head1 NAME

Text::Glob - translate glob patterns to regexes

=head1 SYNOPSIS

    use Text::Glob;

    my $t = Text::Glob.new;
    my $regex = $t.glob_to_regex('*.pm');

=head1 DESCRIPTION

Perl 6 port of the C<Text::Glob> library.

=head1 METHODS

=over 4

=item glob_to_regex

Takes a glob pattern and translates it to a regex.

=back

=head1 AUTHOR

Sebastian Riedel <sri@oook.de>

Based upon C<Text::Glob> by Richard Clamp

=head1 LICENSE

This library is free software . You can redistribute it and/or modify
it under the same terms as perl itself.

=cut
