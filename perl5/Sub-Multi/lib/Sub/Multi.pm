package Sub::Multi;
our $VERSION = '0.003';
use 5.008;
use base 'Class::Multimethods::Pure';
use Data::Bind 0.27;

=head1 NAME

Sub::Multi - Data::Bind-based multi-sub dispatch

=head1 SYNOPSIS

 my $multi_sub = Sub::Multi->new($sub1, $sub2, $sub3);

Now dispatch to the right subroutine, based on C<@args>.

 $multi_sub->(@args);

=head1 DESCRIPTION

Perl6 allows multiple subs and methods with the same name, differing only in
their signature.

    multi sub bar (Dog $foo) {?}
    multi sub bar (Cat $foo) {?}

Dispatching will happen based on the runtime signature of the subroutine or
method call. 

=head2 new

 my $multi_sub = Sub::Multi->new($sub1, $sub2, $sub3);
 $multi_sub->(@args);

Build and return a code reference that will dispatch based on the Perl6
multi dispatch semantics. 

I<TODO: Verify this statement:> Before the method is actually dispatched, 
a call to Data::Bind->sub_signature should be made to register the subroutine
signature. 

=cut


sub new {
    my ($class, @subs) = @_;
    return bless sub { $class->dispatch(\@subs, @_) }, 'Sub::Multi::Method';
}

=head2 add_multi

  my $multi_sub  =  Sub::Multi->add_multi($sub_name, \&sub );
  $multi_sub->(@args);

Associates C<$sub_name> with C<\&sub>, and returns code reference
that will dispatch appropriately. C<add_multi> can be called multiple
times with the same C<$sub_name> to build a multi-dispatch method. 

I<TODO: Verify this statement:> Before the method is actually dispatched, 
a call to Data::Bind->sub_signature should be made to register the subroutine
signature. 

=cut

sub add_multi {
    my ($class, $name, $sub) = @_;
    my $pkg = ((caller)[0]);
    no strict 'refs';
    my $subs = ${$pkg."::SUB_MULTI_REGISTRY"} ||= [];
    push @$subs, $sub;
    no warnings 'redefine';
    *{$pkg."::$name"} = $class->new(@$subs);
}

sub dispatch {
    my $class = shift;
    my $subs = shift;
    my @compat;
    for my $variant (@$subs) {
	my $cv = Data::Bind::_get_cv($variant);
	push @compat, $variant if *$cv->{sig}->is_compatible( [ @{$_[0]} ], { %{$_[1]} } );
    }
    die 'I hate vapour ware' unless @compat;
    while (@compat != 1) {
	die 'I hate ambiguous software';
    }
    goto $compat[0];
}

1;

=head1 SEE ALSO

L<Data::Bind>

B<TODO: > Add a good reference to Perl6 multiple dispatch here. 

=head1 AUTHORS

Chia-liang Kao E<lt>clkao@clkao.orgE<gt>

=head1 COPYRIGHT

Copyright 2006 by Chia-liang Kao and others.

This program is free software; you can redistribute it and/or modify it
under the same terms as Perl itself.

See L<http://www.perl.com/perl/misc/Artistic.html>

=cut
