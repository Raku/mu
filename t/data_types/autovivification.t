use v6-alpha;

use Test;

plan 6;

{
    # L<S09/Autovivification/In Perl 6 these read-only operations are indeed non-destructive:>
    my %a;
    my $b = %a<b><c>;
    is %a.keys.elems, 0, 'fetching doesn't autovivify.';
}

{
    # L<S09/Autovivification/In Perl 6 these read-only operations are indeed non-destructive:>
    my %a;
    my $b = exists %a<b><c>;
    is %a.keys.elems, 0, 'exists doesn't autovivify.';
}


{
    # L<S09/Autovivification/But these ones do autovivify:>
    my %a;
    my $b := %a<b><c>;
    is %a.keys.elems, 1, 'binding autovivifies.';
}

{
    # L<S09/Autovivification/But these ones do autovivify:>
    my %a;
    my $b = \%a<b><c>;
    is %a.keys.elems, 1, 'capturing autovivifies.';
}

{
    # L<S09/Autovivification/But these ones do autovivify:>
    my %a;
    foo(%a<b><c>);
    is %a.keys.elems, 1, 'in arguments autovivifies.';
}

{
    # L<S09/Autovivification/But these ones do autovivify:>
    my %a;
    %a<b><c> = 1;
    is %a.keys.elems, 1, 'store autovivify.';
}


sub foo {
    # just some random subroutine.
}
