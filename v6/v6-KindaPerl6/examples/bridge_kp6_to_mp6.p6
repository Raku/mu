use Kp6Ast; # Kp6Ast should be KindaPerl6::Ast compiled with kp6
say "package Call;";
for ((Call.HOW).methods()) -> $method {
    say "sub $method {
        ::DISPATCH(@_);
    }";
}
for ((Call.HOW).attributes()) -> $attr {
    say "sub $attr {@_ == 1 ? ::DISPATCH(\$_[0],\"$attr\") : ::DISPATCH(::DISPATCH(\$_[0],\"$attr\"),'STORE',\$_[1])}";
}

#package Call;
#sub new { shift; bless {@_}, "Call" }
#sub invocant  { @_ == 1 ? ( $_[0]->{invocant} )  : ( $_[0]->{invocant}  = $_[1] ) }


=begin

=head1 AUTHORS

The Pugs Team E<lt>perl6-compiler@perl.orgE<gt>.

=head1 SEE ALSO

The Perl 6 homepage at L<http://dev.perl.org/perl6>.

The Pugs homepage at L<http://pugscode.org/>.

=head1 COPYRIGHT

Copyright 2007 by Flavio Soibelmann Glock and others.

This program is free software; you can redistribute it and/or modify it
under the same terms as Perl itself.

See L<http://www.perl.com/perl/misc/Artistic.html>

=end
