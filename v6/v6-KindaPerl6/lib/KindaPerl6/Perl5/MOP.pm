
use v5;

use KindaPerl6::Perl5::Type;

{
package Class; # virtual
}

{
package KindaPerl6::MOP;

    our @ISA = ( 'Type_Constant', 'Class' );
    our %Classes;
    
    sub _mangle {
        my $name = shift;
        $name =~ s/ ([^a-zA-Z0-9_:] | (?<!:):(?!:)) / '_'.ord($1).'_' /xge;
        $name;
    }
    sub perl { 
        bless \( do{ my $v = 
            'class { ... }' 
        } ), 'Type_Constant_Buf' 
    }
    sub HOW  { $_[0] }
    sub create {
        my ( $self, $name ) = @_;
        # ??? Type_...
        my $unboxed_name = ${$name->FETCH};
        $native_name = _mangle( ${$name->FETCH} || 'Class_ANON_' . rand );
        #print "Class.create $native_name\n";
        my $class;
        $class = exists $Classes{ $unboxed_name }
            ? $Classes{ $unboxed_name }
            : $Classes{ $unboxed_name } = bless {
                  class_name => $name,
                  class_native_name => $native_name,
                  methods    => { }, 
                  attributes => { },
              }, __PACKAGE__;
        $class->add_method(
            bless( \( do{ my $v = 
                    'HOW' 
                } ), 
                'Type_Constant_Buf' 
            ),
            sub { $class },
        );
        $class->add_method(
            bless( \( do{ my $v = 
                    'new' 
                } ), 
                'Type_Constant_Buf' 
            ),
            sub { 
                #require Data::Dump::Streamer;
                #print "new: ", Data::Dump::Streamer::Dump( @_ );
                my $self = shift; 
                my %data;
                while ( @_ ) {
                    my ( $key, $value ) = ( shift, shift );
                    $data{ ${$key->FETCH} } = $value->FETCH;
                }
                bless \%data, $self; 
            },
        );
    }
    sub add_method {
        my ( $class, $name, $code ) = @_;
        my $unboxed_name = ${$name->FETCH};
        my $native_name = _mangle( ${$name->FETCH} );
        #print "Class.add_method $class->{class_native_name} $native_name\n";
        $class->{methods}{$unboxed_name} = {
            method_name => $name,
            method_native_name => $native_name,
            code        => $code,
        };
        eval "
            package $class->{class_native_name};
            *$native_name = \$code;
            " or warn $@;
    }
    sub add_attribute {
        my ( $class, $name ) = @_;
        my $unboxed_name = ${$name->FETCH};
        my $native_name = _mangle( ${$name->FETCH} );
        #print "Class.add_attribute $class->{class_native_name} $native_name\n";
        my $code = sub {
            @_ == 1 ? ( $_[0]->{$unboxed_name} ) : ( $_[0]->{$unboxed_name} = $_[1] ) 
        };
        $class->{attributes}{$unboxed_name} = {
            attribute_name => $name,
            attribute_native_name => $native_name,
            code => $code,
        };
        eval "
            package $class->{class_native_name};
            *$native_name = \$code;
            " or warn $@;
    }
}

1;

__END__

{
    package KindaPerl6::MOP;
    use Class::MOP;
    use base 'Class::MOP::Class';
    
    eval {
        # respond to p6 'autobox'
        $_->add_method("FETCH" => sub { @_ } );
        #$_->alias_method("HOW" => sub { (shift)->meta }) 
    } foreach Class::MOP::get_all_metaclass_instances;
    
    sub create {
        my $class = shift;
        my $new_class = shift;
        $new_class = $$new_class
            if ref $new_class; # unbox to p5 if needed
        my $meta = $class->SUPER::create( $new_class );
        $meta->alias_method("HOW" => sub { (shift)->meta }); 
        
        $meta->add_method("new" => sub { 
                 my ($class, %param) = @_;
                 $class->meta->new_object(%params);
             } );
        # respond to p6 'autobox'
        #$meta->add_method("FETCH" => sub { @_ } );

        return $meta;    
    }
    #sub FETCH { @_ }
}

1;
