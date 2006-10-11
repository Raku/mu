
package Perl6::ContainerTypes;

use strict;
use warnings;

use Perl6::MetaModel;

role 'IScalar' => {};

class 'PIL::Run::Container::Scalar' => {
    does => [ 'IScalar' ],
    instance => {
        attrs => [ '$:value' ],
        methods => {
            'scalar_fetch' => sub {
                (shift)->get_value('$:value');
            },
            'scalar_store' => sub {
                my ($self, $value) = @_;
                $self->set_value('$:value' => $value);                        
            },
            'scalar_const' => sub {
                0;
            },
        }        
    }
};

1;
