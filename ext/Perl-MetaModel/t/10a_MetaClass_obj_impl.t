#!/usr/bin/pugs

use v6;
use Test;

plan 35;

class Perl::MetaClass {
    has $.name;
    has $.super;
    has @.subclasses;
    has %.properties;
    has %.methods;
    has %.assoc;
    
    method clsName (Str ?$name) returns Str {
        $.name = $name if $name.defined;
        return $.name;
    }
    
    method clsIsa ($self: $class) returns Bool {
        # recurse if we are given a $class instance and not a Class name
        return $self.clsIsa($class.clsName()) if $class.isa('Perl::MetaClass');
        # if the class name itself matches the class return true
        return 1 if $self.clsName() eq $class;
        # now go up the hierarchy ...
        my $super = $self.clsSuper();
        # return false if the $inv has no superclass
        return 0 unless $super.defined;
        # if the superclass is equal to class then return true
        return 1 if $super.clsName() eq $class; 
        # if not, check against the superclass 
        return $super.clsIsa($class);
    }       

    method clsSuper ($self: ?$super) {
        if $super.defined {
            # NOTE:
            # we enforce the following rule on superclasses:
            # - it must be an instance of Perl::MetaClass
            # - the intended super class cannot itself inherit 
            #   from the invocant (circular inheritance)
            ($super.isa('Perl::MetaClass'))
                || die "Super class must be a Perl::MetaClass instance";   
            (!$super.clsIsa($self))
                || die "The super class cannot inherit from the invocant (circular inheritance)";            
            # if super is being changed ...
            if $.super.defined {
                # we need to remove the invocant 
                # from the super's list of subclasses
                $.super._removeSubClass($self);
            }
            # ... and now we can set super    
            $.super = $super;
            # now add the invocant to the super's subclasses
            $super.clsSubClasses($self);
        }
        return $.super;
    }
    
    sub _removeSubClass($self: $subclass) returns Void {
        my $count = 0;
        for @.subclasses -> $my_subclass {
            last if $subclass =:= $my_subclass;
            $count++;
        }
        @.subclasses.delete($count);
    } 
    
    sub clsSubClasses($self: Array *@subclasses) returns Array {
        if @subclasses {       
            # NOTE:
            # enforce the following rules on all @subclasses:
            # - the subclass is an instance of Perl::MetaClass
            # - if the subclass has a superclass, it's superclass is our invocant
            # - if the invocant has a superclass, the subclass is not the superclass of our invocant
            for @subclasses -> $subclass is rw {
                ($subclass.isa('Perl::MetaClass'))
                    || die "Sub class must be a Perl::MetaClass instance (got: '$subclass')"; 
                my $subclass_super = $subclass.clsSuper();
                ($subclass_super && $subclass_super.clsName() eq $self.clsName())
                    || die "Sub class's superclass must be the invocant (got: '{ $subclass.clsSuper() }')";
            }
            @.subclasses.push(@subclasses);                           
        }
        return @.subclasses;
    }       

}

# --------------------------------------------------------------
# This test attempts to model the following class.
# --------------------------------------------------------------
#
#         Package        <- Super-MetaClass
#            |
#          Role          <- MetaClass
#            |
#    [ .prop1 .prop2 ]   <- Properties
#            |
#   [ method1 method2 ]  <- Methods
#            |
#          Class         <- SubClasses
#
# --------------------------------------------------------------

my $role = Perl::MetaClass.new(:name<Role>);

is($role.clsName(), 'Role', '... we got the right class name');
ok($role.clsIsa('Role'), '... $role is-a Role');
ok($role.clsIsa($role), '... $role is-a $role');

# Super Class

my $package = Perl::MetaClass.new(:name<Package>);

is($role.clsSuper(), undef, '... we do not have a superclass');
ok($package.clsIsa('Package'), '... $package is-a Package');

$role.clsSuper($package);

{
    my $super = $role.clsSuper();
    is($super.clsName(), 'Package', '... we now have a superclass');
}

ok($role.clsIsa('Package'), '... $role is-a Package');
ok($role.clsIsa($package), '... $role is-a $package');

# confirm the circular reference ...

my $subclasses = $package.clsSubClasses();
is(+$subclasses, 1, '... $package has one subclass');
{
    my $subclass = $subclasses[0];
    isa_ok($subclass, 'Perl::MetaClass');
    ok($subclass =:= $role, '... and it is the $role');
}

# make sure we *dont* do circular inheritence

dies_ok {
    $package.clsSuper($role);
}, '... cannot make a subclass into a superclass';
like($!, rx:perl5/^The super class cannot inherit from the invocant \(circular inheritance\)/, '... got the right error');

# Sub Classes

{
    my $subclasses = $role.clsSubClasses();
    is(+$subclasses, 0, '... no subclasses yet');
}

my $class = Perl::MetaClass.new(:name<Class>);
$class.clsSuper($role);

{
    my $subclasses = $role.clsSubClasses();
    is(+$subclasses, 1, '... we have 1 subclasses now');
    my $subclass1 = $subclasses[0];
    is($subclass1.clsName(), 'Class', '... this is our first subclass');
}

ok($class.clsIsa('Class'), '... $class is-a Class');
ok($class.clsIsa($class), '... $class is-a $class');

ok($class.clsIsa('Role'), '... $class is-a Role');
ok($class.clsIsa($role), '... $class is-a $role');

ok($class.clsIsa('Package'), '... $class is-a Package');
ok($class.clsIsa($package), '... $class is-a $package');

ok(!$class.clsIsa('Nothing'), '... $class is-not-a Nothing');
ok(!$role.clsIsa('Nothing'), '... $role is-not-a Nothing');
ok(!$package.clsIsa('Nothing'), '... $package is-not-a Nothing');

# change the superclass
$class.clsSuper($package);

{ # role should no longer have the $class as a subclass
    my $subclasses = $role.clsSubClasses();
    is(+$subclasses, 0, '... no subclasses for $role anymore');
}

{ # and it should be in $package now
    my $subclasses = $package.clsSubClasses();
    my @_subclasses = ($subclasses[0], $subclasses[1]);
    my @subclasses = @_subclasses.sort:{ $^a.clsName() cmp $^b.clsName() };
    is(+@subclasses, 2, '... we now have 2 subclasses in $package');
    my $subclass1 = @subclasses[0];
    is($subclass1.clsName(), 'Class', '... $class is a subclass');        
    my $subclass2 = @subclasses[1];
    is($subclass2.clsName(), 'Role', '... as is $role');
}

# change it back now
$class.clsSuper($role);

# now everything is back to "normal"
{
    my $subclasses = $role.clsSubClasses();
    is(+$subclasses, 1, '... 1 subclass for $role again');
    my $subclass1 = $subclasses[0];
    is($subclass1.clsName(), 'Class', '... $class is a subclass of $role again');        
}

{
    my $subclasses = $package.clsSubClasses();
    is(+$subclasses, 1, '... we now have 1 subclass in $package again');
    my $subclass1 = $subclasses[0];
    is($subclass1.clsName(), 'Role', '... and it is $role');
}

dies_ok {
    $class.clsSubClasses($role);
}, '... subclass cannot be the superclass of the invocant';
like($!, rx:perl5/^Sub class\'s superclass must be the invocant/, '... got the right error');

