module Getopt::Proccesor {
    class Getopt::Processor {
        has $argument_class = ::Getopt::Argument::Style;
        has $normalizer_role = ::Getopt::Argument::Normalizer::Null;
        has $coercer_role = ::Getopt::Argument::Coercer::Null;

        method bind ( Code &fun, Array of Str @args ) {
            $fun.assuming( *self.parse_arguments( @args ) );
        }

        method parse_arguments ( Array of Str @args ) {
            collect_arguments( self.create_arguments( @args ) );
        }

        my multi sub collect_arguments ([]) {}
        my multi sub collect_arguments ([ $args, *@args ]) {
            my $normalizer = $arg but $.normalizer_role;
            return $normalizer but $.coercer_role, collect_arguments( grep { not try { $normalizer.subsume( $_ ) } } @args );
        }
        
        method create_arguments ( @strings )  {
            $.argument_class.string_to_arguments( @strings );
        }
    }

    class Getopt::Argument::Style {
        has $value;

        method ^string_to_arguments ( $string ) {
            return $?CLASS.new( $string );
        }
        
        method accept ( $arg ) {  }
    }

    role Getopt::Argument::Normalizer::Null { }

    role Getopt::Argument::Coercer::Null { }
}

module Getopt::Emitter::Perl {
    role Getopt::Argument::Normalizer::Perl {
        has Str $name;
        has Str $string;
        has Int $count = 0;
        has Array of Str @strings;

        method subsume ( Getopt::Argument $arg ) { 
            $.append( $self.accept( $arg ) );
        }
        
        method append ( Getopt::Argument $arg ) {
            $.append_name( $arg );
            $.append_value( $arg );
            $.count++;
        }

        method append_name ( $arg ) {
            $.name ||= $arg.name;
            push @.names, $arg.name;
        }
        
        method append_value ( $arg ) {
            $.string = $arg.value;
            push @.strings, $arg.value;
        }
    }


    role Getopt::Argument::Coercer::Perl {
        multi method as (--> Num) { defined($.string) ?? Num $.string !! $.count }
        multi method as (--> Str) { $.string }
        multi method as (--> Bool) { true }
        multi method as (--> Array) { @.strings }
        multi method as (--> Named) { $.name => self }
    }

    class Getopt::Argument::Style::Clustered is Getopt::Argument::Style {
        method ^string_to_arguments( $string ) {
            given $string {
                when /^-(\w)+/ { map { call("-$_") } @/ }
                default { next METHOD }
            }
        }
    }
}
