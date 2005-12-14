package PIL2JS::JSPM;
use strict;
use warnings;

use JavaScript;
use UNIVERSAL::require;

# XXX: not yet
sub _map_method {
    my $class = shift;

    require Devel::Symdump;
    my $obj = Devel::Symdump->new($class);

    for ($obj->functions) {
#	warn "==> to export $_";
    }
    return {};
}

sub perl_use_class {
    my $ct = shift;
    my $class = shift;
    $class->require;

    my $jsclass = $class;
    $jsclass =~ s/_/__/g;
    $jsclass =~ s/::/_/g;

    $ct->bind_class
	( name => $jsclass,
	  constructor => sub {},
	  flags => JS_CLASS_NO_INSTANCE,
	  method => _map_method($class),
	  package => $class );

    $class->import(@_);
}


sub init_js_for_perl5 {
    my $ct = shift;

    $ct->bind_class(
        name => 'Perl5',
		constructor => sub {},
		flags => JS_CLASS_NO_INSTANCE,
        methods => {
		    perl_eval => sub { shift; eval($_[0]) },
		    perl_use => sub { shift; perl_use_class($ct, @_) },
		    perl_can => sub { shift; my $func = $_[0]->can($_[1]); },
        },
        package => 'Perl5',
    );


    my $perl5 = bless {}, 'Perl5';
    $ct->bind_object('Perl5', $perl5);

    use Data::Dumper;
    $ct->bind_function(
		   name => 'alert',
		   func => sub { warn Dumper(@_) });
    my $code = <<EOC;

function perl_apply() {
    var args = new Array();
    for (var i = 0; i < arguments.length; i++) {
        args.push(arguments[i]);
    }

    var func = args.shift();
    return func.apply(func,args);
}

function perl_get_method(obj, func) {
    return Perl5.perl_can(obj, func);
}

function perl_call_method() {
    var args = new Array();
    for (var i = 0; i < arguments.length; i++) {
        args.push(arguments[i]);
    }
    var obj = args[0];
    var method = perl_get_method(obj, args.splice(1,1)[0] );

    return method.apply(method, args)
}

1
EOC

    $ct->eval($code) or die;

    $ct->set_error_handler(sub {
	my ($message, $lineno, $linebuff) = @_;
	die "Error at line: $lineno\nMessage is: $message\nSource is: $linebuff\n";
    });
}

=cut

=head1 AUTHORS

Chia-liang Kao E<lt>clkao@clkao.orgE<gt>

=head1 COPYRIGHT

Copyright 2005 by Chia-liang Kao E<lt>clkao@clkao.orgE<gt>.

This program is free software; you can redistribute it and/or modify
it under the same terms as Perl itself.

See L<http://www.perl.com/perl/misc/Artistic.html>

=cut

1;
