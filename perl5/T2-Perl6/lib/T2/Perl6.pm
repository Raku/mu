
package T2::Perl6;

use T2::Schema;
use Perl6::MetaModel;

use base qw(Class::Tangram);

our $VERSION = 0.01;

# an example of a Tangram schema structure
our $schema =
    { fields =>
      { ref =>
        { schema => { class => "T2::Schema" }
        },
      },
      methods => { map { $_ => \&$_ }
                   qw( generate_p6mm generate_attribute )
                 },
    };

sub generate_p6mm {
    my $self = shift;

    my $schema = $self->schema;

    for my $class ( $schema->classes ) {

        class($class->name =>
              { instance =>
                { methods => { map { $_->name => $_->code }
                               $class->methods->members
                             },

                  # Perl 6 has no concept of associative attributes,
                  # so we represent them all as attributes
                  attrs => [ map { $self->generate_attribute($_) }
                             $class->attributes->members,
                             $class->associations->members,
                             # 2-way associations won't work without
                             # MM support (or proxy accessors)
                             # $self->rev_assocs->members,
                           ],
                },
              });
    }
}

# ok, so Class::Tangram doesn't have a real type system :-P
# there are more types ... but this will do for now
our %types
    = ( int => "Int",
        real => "Num",
        string => "Str",
        flat_hash => "Hash",   # of Scalar
        flat_array => "Array", # ditto
        ref => "Object",   # for now
        set => "Set", iset => "Set",         # also for now
        hash => "Hash", ihash => "Hash",     #  └── "" ──┘
        array => "Array", iarray => "Array", #  └── "" ──┘
      );

our %sigils
    = ( (map { $_ => '%' } qw(flat_hash hash ihash)),
        (map { $_ => '@' } qw(flat_array array iarray)),
      );

sub generate_attribute {
    my $self = shift;
    my $attr = shift;

    # $attr may be a T2::Attribute or a T2::Association, but as they
    # both respond to ->type and ->name in the same way, we can use
    # them here.

    my $sigil = $sigils{$attr->type} || '$';

    my $name = $sigil.".".$attr->name;

    if ( my $perl6_type = $type_lookup{$attr->type} ) {
        return [ $name => { type => $perl6_type,
                            access => "rw",
                          } ];
    }
    else {
        return $name;
    }
}

1;
