use warnings;
use strict;
no strict 'refs';

package Cached;
sub AUTOLOAD {
    my $method_name = our $AUTOLOAD;
    my ($self,@args) = @_;
    my $cachid = Cache::get_cacheid($self);
    bless($self,"Cache::Foo");
    *{"Cache::Foo::AUTOLOAD"} = \&Cache::autoload;
    $method_name =~ s/^.*::(\w+)$/$1/;
    $self->$method_name(@args);
}
package Cache;
sub autoload {
    our $AUTOLOAD;
    my $self=shift;
    my $method_name=$AUTOLOAD;
    return if $AUTOLOAD eq ref($self) . '::DESTROY';
    $method_name =~ s/^.*::(\w+)$/$1/;
    print "#AUTOLOAD $AUTOLOAD\n";
    my $method = get_method($self,$method_name);
    *{$AUTOLOAD} = $method;
    $method->(@_);
}
sub get_cacheid {
    return 'Foo';
}
sub get_method {
    my ($object,$method_name) = @_;
    die "no such method $method_name" if $method_name ne 'foo';
    return sub {
        print "foo\n";
    };
}
package main;
use autobox HASH=>'Cached';
my $ref = {};
$ref->foo();
$ref->foo();
