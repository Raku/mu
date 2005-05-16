
use v6;

class Perl::Meta::MetaClass-0.0.1;

use Set;

sub Perl::Meta::MetaClass::new (?$name) returns Perl::Meta::MetaClass is export {
    return Perl::Meta::MetaClass.new(
        name       => $name,
        subclasses => set(),       
    );
}

has $:name;
has $:parent;
has Set $:subclasses;
has %:properties;
has %:methods;

submethod BUILD($:name, $:subclasses) {}

method clsName ($self: ?$name) returns Str {
    $:name = $name if $name.defined;
    return $:name;
}

method clsIsa ($self: Any $class) returns Bool {
    # recurse if we are given a $class instance and not a Class name
    return $self.clsIsa($class.clsName()) if $class.ref() eq 'Perl::Meta::MetaClass';
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

method clsSuper ($self: Perl::Meta::MetaClass ?$super) returns Perl::Meta::MetaClass {
    if $super.defined {   
        # NOTE:
        # we enforce the following rule on superclasses:
        # - the intended super class cannot itself inherit 
        #   from the invocant (circular inheritance)    
        (!$super.clsIsa($self))
            || die "The super class cannot inherit from the invocant (circular inheritance)";            
        # if the parent is defined, then we 
        # are actually changing it, which means
        if $:parent.defined {
            # we need to remove the invocant 
            # from the super's list of subclasses        
            $:parent._removeSubClass($self);
        }
        # ... and now we can set super          
        $:parent = $super;
        # now add the invocant to the super's subclasses        
        $super.clsSubClasses($self);        
    }
    return $:parent;
}

method _removeSubClass ($self: Perl::Meta::MetaClass $subclass) returns Void {
    $:subclasses.remove($subclass);
}

method clsSubClasses ($self: *@subclasses) returns Set {
    if @subclasses {       
        # NOTE:
        # enforce the following rules on all @subclasses:
        # - the subclass is an instance of Perl::MetaClass
        # - if the subclass has a superclass, it's superclass is our invocant
        # - if the invocant has a superclass, the subclass is not the superclass of our invocant
        for @subclasses -> $subclass is rw {
            ($subclass.ref() eq 'Perl::Meta::MetaClass')
                || die "Sub class must be a Perl::MetaClass instance (got: '$subclass')"; 
            ($subclass.clsSuper() && $subclass.clsSuper().clsName() eq $self.clsName())
                || die "Sub class's superclass must be the invocant (got: '{ $subclass.clsSuper() }')";                          
        }
        $:subclasses.insert(@subclasses);        
    }
    return $:subclasses;    
}

=pod

=head1 NAME

Perl::Meta::MetaClass - A meta-meta-model for Perl Classes

=head1 SYNOPSIS

  #         Package
  #          |
  #    +-----+----+
  #    |          |
  #  Module      Role
  #               |
  #             Class
  
  my $package = Perl::Meta::MetaClass.new('Package');
  my $role = Perl::Meta::MetaClass.new('Role');  
  my $module = Perl::Meta::MetaClass.new('Module');    
  
  $role.clsSuper($package);
  $module.clsSuper($package);  
  
  my $class = Perl::Meta::MetaClass.new('Class');  
  $class.clsSuper($role);  
  
  $class.clsIsa('Package');

=head1 DESCRIPTION

Perl::Meta::MetaClass is the meta-model of the Perl6 object system. The code 
in this module itself is the meta-meta-model of the Perl6 object system.

=head1 NOTES ON USAGE

=head2 Modeling Inheritance

Inheritance relationships are best defined using C<.clsSuper> rather that C<.clsSubClasses>.
Currently C<.clsSuper> will deal with all subclass related issues automagically, while 
C<.clsSubClasses> will not allow a subclass to be assigned unless already has it's superclass
assigned (which would have already taken care of the subclass, making the entire action 
redundant anyway).

So this means that this is the proper way to set up an inheritance relationship:

  my $package = Perl::Meta::MetaClass.new('Package');
  my $role = Perl::Meta::MetaClass.new('Role');  

  $role.clsSuper($package);
  
And this code would fail:

  my $package = Perl::Meta::MetaClass.new('Package');
  my $role = Perl::Meta::MetaClass.new('Role');  

  $package.clsSubClasses($role);  # << this will die
  
And this would just be redundant: 

  my $package = Perl::Meta::MetaClass.new('Package');
  my $role = Perl::Meta::MetaClass.new('Role');  

  $role.clsSuper($package);
  $package.clsSubClasses($role); # << this is redundant, it is already done automagically


=head1 METHODS

=over 4

=item B<clsName($self: ?$name)>

=item B<clsSuper($self: ?$superclass)>

=item B<clsSubClasses($self: *@subclasses)>

=item B<clsIsa ($self: $class)>

This accepts both a Class name and C<$class> instance.

=back

=head1 SEE ALSO

See the L<Perl::MetaModel> module for more details.

See the F<docs/meta_meta_classes.pod> file as well.

=head1 AUTHORS

Sam Vilain

Stevan Little E<lt>stevan@iinteractive.comE<gt>

=cut
