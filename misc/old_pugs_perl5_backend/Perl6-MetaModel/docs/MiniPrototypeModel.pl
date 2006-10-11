#!/usr/bin/perl

use strict;
use warnings;

use Carp 'confess';
use Test::More tests => 30;

=pod

Mini-Prototype-Model

This is a very basic prototype based object system, it was inspired by
a purely functional prototype OO system written in Scheme, which can
be seen here:

  http://okmij.org/ftp/Scheme/pure-oo-system.scm

I ended up removing the "purely functional" aspects of it in favor of 
a more practical closure based system, similar to what is described
in here (note: same author as the first scheme code):

  http://okmij.org/ftp/Scheme/oop-in-fp.txt
  
This system is still relatively "functional" in flavor, however, it is
not pure since it uses assignment.  

SEE ALSO:

  http://okmij.org/ftp/Scheme/#pure-oo

=cut

# new() creates instances of classes
sub new ($) {
    my ($table) = @_;
    my $self;
    $self = bless sub {
        my $message = shift;
        return $table  if $message eq 'message-map';        
        
        # alias prototype to some common names for super
        return $table->{prototype}
            if ($message eq 'prototype' ||
                $message eq 'super'     ||
                $message eq 'parent'    ) &&
                exists $table->{prototype};      
        
        my $_table = $table;  
    TRY_AGAIN:
        if (my $method = $_table->{$message}) {
            return $method->($self, @_);
        }
        else {
            if (exists $_table->{prototype}) {
                $_table = $_table->{prototype}->('message-map');
                goto TRY_AGAIN;
            }
        }
        confess "could not find message ($message)";
    } => 'Dispatchable';
}

{
    package Dispatchable;
    sub isa { our $AUTOLOAD = 'isa'; goto &AUTOLOAD; }
    sub can { our $AUTOLOAD = 'can'; goto &AUTOLOAD; }    
    sub AUTOLOAD {
        my ($label) = (split '::' => our $AUTOLOAD)[-1];
        return if $label =~ /DESTROY/;
        my $self = shift;
        return $self->($label, @_);
    }
}

## ----------------------------------------------------------------------------
## Classes

sub Object { 
    { 
        id    => sub { 0+(shift) },
        class => sub { \&Object  },
    };
}

sub Point {
    my ($x, $y) = @_;
    return {
    # prototype
        prototype => new Object(),
    # class definition
        get_x => sub { $x },
        get_y => sub { $y },  
        set_x => sub {
            my ($self, $new_x) = @_;
            $x = $new_x;
        },                  
        set_y => sub {
            my ($self, $new_y) = @_;
            $y = $new_y;
        },    
        clear => sub {
            $x = 0; $y = 0;
        },   
        class => sub { \&Point },        
    };
}

my $p = new Point(5, 6);
is($p->class, \&Point, '... got the right class name');
is($p->prototype->class, \&Object, '... got the right class name');

is($p->get_x, 5, '... got the right x for p');
is($p->get_y, 6, '... got the right y for p');

my $p1 = new Point(15, 16);
is($p1->class, \&Point, '... got the right class name');

is($p1->get_x, 15, '... got the right x for p1');
is($p1->get_y, 16, '... got the right y for p1');

cmp_ok($p->id, '!=', $p1->id, '... got different ids');

my $old_p_id = $p->id;

$p->set_x(100);
is($p->get_x, 100, '... got the right x for p2');
cmp_ok($p->id, '==', $old_p_id, '... got the same ids');

my $p2 = new $p1->class->(100, 200);
is($p2->class, \&Point, '... got the right class name');

is($p2->get_x, 100, '... got the right x for p2');
is($p2->get_y, 200, '... got the right y for p2');

$p->clear;
is($p->get_x, 0, '... got the right x for p');
is($p->get_y, 0, '... got the right y for p');

is($p2->get_x, 100, '... still got the right x for p2');
is($p2->get_y, 200, '... still got the right y for p2');
is($p1->get_x, 15, '... still got the right x for p1');
is($p1->get_y, 16, '... still got the right y for p1');

sub Point3D {
    my ($x, $y, $z) = @_;
    {
    # prototype
        prototype => new Point($x, $y),  
    # rest of the class 
        get_z => sub { $z },
        set_z => sub {
            my ($self, $new_z) = @_;
            $z = $new_z;
        }, 
        clear => sub {
            my $self = shift;
            $z = 0;
            $self->super->clear;
        },              
        class => sub { \&Point3D },        
    };
}

my $p3d = new Point3D(5, 6, 7);
is($p3d->class, \&Point3D, '... go the right class name for Point3D');
is($p3d->prototype->class, \&Point, '... go the right superclass name for Point3D');
is($p3d->prototype->prototype->class, \&Object, '... go the right super-superclass name for Point3D');

is($p3d->get_x, 5, '... got the right x for p3d');
is($p3d->get_y, 6, '... got the right y for p3d');
is($p3d->get_z, 7, '... got the right z for p3d');

cmp_ok($p3d->id, '!=', $p->id, '... not the same ids');

$p3d->set_x(200);
is($p3d->get_x, 200, '... got the right x for p3d');

$p3d->clear;
         
is($p3d->get_x, 0, '... got the right cleared x for p3d');
is($p3d->get_y, 0, '... got the right cleared y for p3d');
is($p3d->get_z, 0, '... got the right cleared z for p3d');

