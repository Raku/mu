
package Perl6::ContainerTypes;

use strict;
use warnings;

use Perl6::MetaModel;

class 'Perl6::Scalar' => {
    does => [ 'Scalar' ],
    class => {
        attrs => [ '$:value' ],
        methods => {
            'FETCH' => {
                (shift)->get_value('$:value');
            },
            'STORE' => {
                my ($self, $value) = @_;
                $self->set_value('$:value' => $value);                        
            }
        }        
    }
};

1;