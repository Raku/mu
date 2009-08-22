use v6-alpha;

class MiniPerl6::Lisp::LexicalBlock {
    has @.block;
    method emit {
        my $str := '';
        my $has_my_decl := 0;
        my $my_decl := '';
        # my $silence_unused_warning := '';
        for @.block -> $decl { 
            if $decl.isa( 'Decl' ) && ( $decl.decl eq 'my' ) {
                $has_my_decl := 1;
                $my_decl := $my_decl ~ '(' ~ ($decl.var).emit ~ ' nil)'; 
                # $silence_unused_warning := $silence_unused_warning ~ ' ' ~ ($decl.var).emit;
            }
            if $decl.isa( 'Bind' ) && ($decl.parameters).isa( 'Decl' ) && ( ($decl.parameters).decl eq 'my' ) {
                $has_my_decl := 1;
                $my_decl := $my_decl ~ '(' ~ (($decl.parameters).var).emit ~ ' nil)'; 
                # $silence_unused_warning := $silence_unused_warning ~ ' ' ~ (($decl.parameters).var).emit;
            }
        }
        if $has_my_decl {
            $str := $str ~ '(let (' ~ $my_decl ~ ') ';

            # silence warning "The variable X is defined but never used." in SBCL
            # $str := $str ~ '(list ' ~ $silence_unused_warning ~ ') ';

        }
        else {
            $str := $str ~ '(progn ';
        };
        for @.block -> $decl { 
            if (!( $decl.isa( 'Decl' ) && ( $decl.decl eq 'my' ))) {
                $str := $str ~ ($decl).emit;
            }
        }; 
        return $str ~ ')';
    }
}


class CompUnit {
    has $.name;
    has %.attributes;
    has %.methods;
    has @.body;
    method emit {

        my $class_name := Main::to_lisp_namespace($.name);
        my $str := ';; class ' ~ $.name ~ Main.newline;

        $str := $str ~ '(defpackage ' ~ $class_name ~ Main.newline
                ~ '  (:use common-lisp))' ~ Main.newline
                ~ ';; (in-package ' ~ $class_name ~ ')' ~ Main.newline;
        # my $silence_unused_warning := '';

        my $has_my_decl := 0;
        my $my_decl := '';
        for @.body -> $decl { 
            if $decl.isa( 'Decl' ) && ( $decl.decl eq 'my' ) {
                $has_my_decl := 1;
                $my_decl := $my_decl ~ '(' ~ ($decl.var).emit ~ ' nil)'; 
                # $silence_unused_warning := $silence_unused_warning ~ ' ' ~ ($decl.var).emit;
            }
            if $decl.isa( 'Bind' ) && ($decl.parameters).isa( 'Decl' ) && ( ($decl.parameters).decl eq 'my' ) {
                $has_my_decl := 1;
                $my_decl := $my_decl ~ '(' ~ (($decl.parameters).var).emit ~ ' nil)'; 
                # $silence_unused_warning := $silence_unused_warning ~ ' ' ~ (($decl.parameters).var).emit;
            }
        }
        if $has_my_decl {
            $str := $str ~ '(let (' ~ $my_decl ~ ')' ~ Main.newline;

            # silence warning "The variable X is defined but never used." in SBCL
            # $str := $str ~ '(list ' ~ $silence_unused_warning ~ ') ';

        }

        $str := $str ~ 
'(if (not (ignore-errors (find-class \'' ~ $class_name ~ ')))
  (defclass ' ~ $class_name ~ ' () ()))

(let (x) 
  (setq x (make-instance \'' ~ $class_name ~ '))
  (defun proto-' ~ $class_name ~ ' () x))
';

        for @.body -> $decl { 
            if $decl.isa( 'Decl' ) && ( $decl.decl eq 'has' ) {
                my $accessor_name := ($decl.var).name;

# suggested by Arthur Lemmens in: http://osdir.com/ml/lisp.lispworks.general/2005-07/msg00153.html 

                $str := $str ~ 
';; has $.' ~ $accessor_name  ~ '
(let ((new-slots (list (list :name \'' ~ Main::to_lisp_identifier($accessor_name)  ~ '
  :readers \'(' ~ Main::to_lisp_identifier($accessor_name)  ~ ')
  :writers \'((setf ' ~ Main::to_lisp_identifier($accessor_name)  ~ '))
  :initform \'nil
  :initfunction (constantly nil)))))
(dolist (slot-defn (sb-mop:class-direct-slots (find-class \'' ~ $class_name  ~ ')))
(push (list :name (sb-mop:slot-definition-name slot-defn)
  :readers (sb-mop:slot-definition-readers slot-defn)
  :writers (sb-mop:slot-definition-writers slot-defn)
  :initform (sb-mop:slot-definition-initform slot-defn)
  :initfunction (sb-mop:slot-definition-initfunction slot-defn))
new-slots))
(sb-mop:ensure-class \'' ~ $class_name  ~ ' :direct-slots new-slots))

';
            }
            if $decl.isa( 'Method' ) {
                my $sig      := $decl.sig;
                my $invocant := $sig.invocant; 
                my $pos      := $sig.positional;
                my $str_specific := '(' ~ $invocant.emit ~ ' ' ~ $class_name ~ ')';
                my $str_generic  :=  $invocant.emit;
                my $str_optionals := '';
                for @$pos -> $field { 
                    $str_optionals := $str_optionals ~ ' ' ~ $field.emit;
                };
                if ( $str_optionals ) {
                    $str_specific := $str_specific ~ ' &optional' ~ $str_optionals;
                    $str_generic  := $str_generic  ~ ' &optional' ~ $str_optionals;
                }
                my $block    := ::MiniPerl6::Lisp::LexicalBlock( block => $decl.block );
                $str := $str ~
';; method ' ~ $decl.name ~ '
(if (not (ignore-errors (find-method \'' ~ Main::to_lisp_identifier($decl.name) ~ ' () ())))
  (defgeneric ' ~ Main::to_lisp_identifier($decl.name) ~ ' (' ~ $str_generic ~ ')
      (:documentation ' ~ '"' ~ 'a method' ~ '"' ~ ')))
(defmethod ' ~ Main::to_lisp_identifier($decl.name) ~ ' (' ~ $str_specific ~ ')
  (block mp6-function
    ' ~ $block.emit ~ '))

';
            }
            if $decl.isa( 'Sub' ) {
                $str := $str 
                    ~ '(in-package ' ~ $class_name ~ ')' ~ Main.newline
                    ~ '  ' ~ ($decl).emit ~ Main.newline
                    ~ '(in-package mp-Main)' ~ Main.newline;
            }
        }; 

        for @.body -> $decl { 
            if    (!( $decl.isa( 'Decl' ) && (( $decl.decl eq 'has' ) || ( $decl.decl eq 'my' )) ))
               && (!( $decl.isa( 'Method'))) 
               && (!( $decl.isa( 'Sub'))) 
            {
                $str := $str ~ ($decl).emit ~ Main.newline;
            }
        }; 
        
        if $has_my_decl {
            # close paren for '(let '
            $str := $str ~ ')';
        }
        $str := $str ~ Main.newline ~ Main.newline;
    }
}

class Val::Int {
    has $.int;
    method emit { $.int }
}

class Val::Bit {
    has $.bit;
    method emit { $.bit }
}

class Val::Num {
    has $.num;
    method emit { $.num }
}

class Val::Buf {
    has $.buf;
    method emit { '"' ~ Main::lisp_escape_string($.buf) ~ '"' }
}

class Val::Undef {
    method emit { 'nil' }
}

class Val::Object {
    has $.class;
    has %.fields;
    method emit {
        'bless(' ~ %.fields.perl ~ ', ' ~ $.class.perl ~ ')';
    }
}

class Lit::Seq {
    has @.seq;
    method emit {
        '(' ~ (@.seq.>>emit).join(' ') ~ ')';
    }
}

class Lit::Array {
    has @.array;
    method emit {
        '(list ' ~ (@.array.>>emit).join(' ') ~ ')';
    }
}

class Lit::Hash {
    has @.hash;
    method emit {
        my $fields := @.hash;
        my $str := '';
        for @$fields -> $field { 
            $str := $str ~ '(setf (gethash ' ~ ($field[0]).emit ~ ' h) ' ~ ($field[1]).emit ~ ')';
        }; 
        '(let ((h (make-hash-table :test \'equal))) ' ~ $str ~ ' h)';
    }
}

class Lit::Code {
    1;
}

class Lit::Object {
    has $.class;
    has @.fields;
    method emit {
        my $fields := @.fields;
        my $str := '';
        for @$fields -> $field { 
            $str := $str ~ '(setf (' ~ Main::to_lisp_identifier(($field[0]).buf) ~ ' m) ' ~ ($field[1]).emit ~ ')';
        }; 
        '(let ((m (make-instance \'' ~ Main::to_lisp_namespace($.class) ~ '))) ' ~ $str ~ ' m)'
    }
}

class Index {
    has $.obj;
    has $.index;
    method emit {
        if $.obj.isa( 'Var' ) {
            return '(elt ' ~ $.obj.name ~ ' ' ~ $.index.emit ~ ')';
            # return '(aref (make-array ' ~ $.obj.name ~ ') ' ~ $.index.emit ~ ')';
            # return '(aref ' ~ $.obj.name ~ ' ' ~ $.index.emit ~ ')';
        };
        return '(elt ' ~ $.obj.emit ~ ' ' ~ $.index.emit ~ ')';
        # return '(aref (make-array ' ~ $.obj.emit ~ ') ' ~ $.index.emit ~ ')';
        # return '(aref ' ~ $.obj.emit ~ ' ' ~ $.index.emit ~ ')';
    }
}

class Lookup {
    has $.obj;
    has $.index;
    method emit {
        if $.obj.isa( 'Var' ) {
            if ($.obj.name eq 'MATCH') || ($.obj.name eq '/') {
                return '(gethash ' ~ $.index.emit ~ ' (sv-hash ' ~ $.obj.emit ~ '))';
            }
            # return '(gethash ' ~ $.index.emit ~ ' ' ~ $.obj.emit ~ ')';
            # return '(gethash ' ~ $.index.emit ~ ' ' ~ $.obj.name ~ ')';
        };
        return '(gethash ' ~ $.index.emit ~ ' ' ~ $.obj.emit ~ ')';
    }
}

class Var {
    has $.sigil;
    has $.twigil;
    has $.name;
    method emit {
        # Normalize the sigil here into $
        # $x    => $x
        # @x    => $List_x
        # %x    => $Hash_x
        # &x    => $Code_x
           ( $.twigil eq '.' )
        ?? ( '(' ~ Main::to_lisp_identifier( $.name ) ~ ' sv-self)' )
        !!  (    ( $.name eq '/' )
            ??   ( Main::to_lisp_identifier( 'MATCH' ) )
            !!   ( Main::to_lisp_identifier( $.name ) )
            )
    };
}

class Bind {
    has $.parameters;
    has $.arguments;
    method emit {
        if $.parameters.isa( 'Lit::Object' ) {

            #  ::Obj(:$a, :$b) := $obj

            my $class := $.parameters.class;
            my $a     := $.parameters.fields;
            my $b     := $.arguments;
            my $str   := 'do { ';
            my $i     := 0;
            my $arg;
            for @$a -> $var {
                my $bind := ::Bind( 
                    'parameters' => $var[1], 
                    'arguments'  => ::Call( invocant => $b, method => ($var[0]).buf, arguments => [ ], hyper => 0 )
                );
                $str := $str ~ ' ' ~ $bind.emit ~ ' ';
                $i := $i + 1;
            };
            return $str ~ $.parameters.emit ~ ' }';
        };
    
        if $.parameters.isa( 'Decl' ) && ( $.parameters.decl eq 'my' ) {
            return '(setf ' ~ ($.parameters.var).emit ~ ' ' ~ $.arguments.emit ~ ')';
        }
        '(setf ' ~ $.parameters.emit ~ ' ' ~ $.arguments.emit ~ ')';
    }
}

class Proto {
    has $.name;
    method emit {
        '(proto-' ~ Main::to_lisp_namespace($.name) ~ ')'
    }
}

class Call {
    has $.invocant;
    has $.hyper;
    has $.method;
    has @.arguments;
    method emit {
        my $invocant := $.invocant.emit;
        if $invocant eq 'self' {
            $invocant := 'sv-self';
        };

        if     ($.method eq 'values')
        { 
            if ($.hyper) {
                die "not implemented";
            }
            else {
                return '@{' ~ $invocant ~ '}';
            }
        };

        if ($.method eq 'perl') {
            return '(prin1-to-string ' ~ $invocant ~ ')';
        };

        if     ($.method eq 'yaml')
            || ($.method eq 'say' )
            # || ($.method eq 'join')
            || ($.method eq 'chars')
            || ($.method eq 'isa')
        { 
            if ($.hyper) {
                return 
                    '[ map { ' ~ $.method ~ '( $_, ' ~ ', ' ~ (@.arguments.>>emit).join(', ') ~ ')' ~ ' } @{ ' ~ $invocant ~ ' } ]';
            }
            else {
                return
                    '(' ~ $.method ~ ' ' ~ $invocant ~ ' ' ~ (@.arguments.>>emit).join(' ') ~ ')';
            }
        };

        my $meth := Main::to_lisp_identifier($.method) ~ ' ';
        if  $.method eq 'postcircumfix:<( )>'  {
             $meth := '';  
        };
        
        if ($.hyper) {
            '(mapcar #\'' ~ $meth ~ $invocant ~ ')';
        }
        else {
            return '(' ~ $meth ~ $invocant ~ ' ' ~ (@.arguments.>>emit).join(' ') ~ ')';
        };

    }
}

class Apply {
    has $.code;
    has @.arguments;
    method emit {
        my $code := $.code;

        if $code eq 'self'       { return 'sv-self' };

        if $code eq 'make'       { return '(return-from mp6-function '   ~ (@.arguments.>>emit).join(' ') ~ ')' };

        if $code eq 'substr'     { return '(sv-substr '   ~ (@.arguments.>>emit).join(' ') ~ ')' };

        if $code eq 'say'        { 
            return '(format t ' ~ '"' ~ '~{~a~}~%' ~ '"' ~ ' (list ' ~ (@.arguments.>>emit).join(' ') ~ '))' };
        if $code eq 'print'      { 
            return '(format t ' ~ '"' ~ '~{~a~}' ~ '"' ~ ' (list ' ~ (@.arguments.>>emit).join(' ') ~ '))' };
        if $code eq 'infix:<~>'  { 
            return '(format nil ' ~ '"' ~ '~{~a~}' ~ '"' ~ ' (list ' ~ (@.arguments.>>emit).join(' ') ~ '))' };
        if $code eq 'warn'       { 
            return '(write-line (format nil ' ~ '"' ~ '~{~a~}' ~ '"' ~ ' (list ' ~ (@.arguments.>>emit).join(' ') 
                    ~ ')) *error-output*)' };
        if $code eq 'die'        { 
            return '(progn (write-line (format nil ' ~ '"' ~ '~{~a~}' ~ '"' ~ ' (list ' ~ (@.arguments.>>emit).join(' ') 
                    ~ ')) *error-output*) (sb-ext:quit))' };

        if $code eq 'array'      { return (@.arguments.>>emit).join(' ') };

        if $code eq 'prefix:<~>' { 
            return '(format nil ' ~ '"' ~ '~{~a~}' ~ '"' ~ ' (list ' ~ (@.arguments.>>emit).join(' ') ~ '))' };
        if $code eq 'prefix:<!>' { return '(not (sv-bool '  ~ (@.arguments.>>emit).join(' ')    ~ ' ))' };
        if $code eq 'prefix:<?>' { return '(sv-bool '  ~ (@.arguments.>>emit).join(' ')    ~ ' )' };

        if $code eq 'prefix:<$>' { return '(sv-scalar ' ~ (@.arguments.>>emit).join(' ')    ~ ')' };

        # if $code eq 'prefix:<@>' { return '@{' ~ (@.arguments.>>emit).join(' ')    ~ '}' };
        if $code eq 'prefix:<@>' { return (@.arguments.>>emit).join(' ') };

        # if $code eq 'prefix:<%>' { return '%{' ~ (@.arguments.>>emit).join(' ')    ~ '}' };
        if $code eq 'prefix:<%>' { return (@.arguments.>>emit).join(' ') };

        if $code eq 'infix:<+>'  { return '(+ '  ~ (@.arguments.>>emit).join(' ')  ~ ')' };
        if $code eq 'infix:<->'  { return '(-'  ~ (@.arguments.>>emit).join(' ')  ~ ')' };
        if $code eq 'infix:<>>'  { return '(> '  ~ (@.arguments.>>emit).join(' ')  ~ ')' };
        if $code eq 'infix:<x>'  { return '(x '  ~ (@.arguments.>>emit).join(' ')  ~ ')' };
        
        if $code eq 'infix:<&&>' { return '(sv-and ' ~ (@.arguments.>>emit).join(' ') ~ ')' };
        if $code eq 'infix:<||>' { return '(sv-or '  ~ (@.arguments.>>emit).join(' ') ~ ')' };
        if $code eq 'infix:<eq>' { return '(sv-eq '  ~ (@.arguments.>>emit).join(' ') ~ ')' };
        if $code eq 'infix:<ne>' { return '(not (sv-eq '  ~ (@.arguments.>>emit).join(' ') ~ '))' };
 
        if $code eq 'infix:<==>' { return '(eql '  ~ (@.arguments.>>emit).join(' ') ~ ')' };
        if $code eq 'infix:<!=>' { return '(not (eql '  ~ (@.arguments.>>emit).join(' ') ~ '))' };

        if $code eq 'ternary:<?? !!>' { 
            return '(if (sv-bool ' ~ (@.arguments[0]).emit ~ ') ' ~ (@.arguments[1]).emit ~ ' ' ~ (@.arguments[2]).emit ~ ')' };
        
        '(' ~ Main::to_lisp_identifier($.code) ~ ' ' ~ (@.arguments.>>emit).join(' ') ~ ')';
    }
}

class Return {
    has $.result;
    method emit {
        return '(return-from mp6-function ' ~ $.result.emit ~ ')';
    }
}

class If {
    has $.cond;
    has @.body;
    has @.otherwise;
    method emit {
        my $block1 := ::MiniPerl6::Lisp::LexicalBlock( block => @.body );
        my $block2 := ::MiniPerl6::Lisp::LexicalBlock( block => @.otherwise );
        '(if (sv-bool ' ~ $.cond.emit ~ ') ' ~ $block1.emit ~ ' ' ~ $block2.emit ~ ')';
    }
}

class For {
    has $.cond;
    has @.body;
    has @.topic;
    method emit {
        my $cond := $.cond;
        my $block := ::MiniPerl6::Lisp::LexicalBlock( block => @.body );
        if   $cond.isa( 'Var' ) 
          && $cond.sigil eq '@' 
        {
            $cond := ::Apply( code => 'prefix:<@>', arguments => [ $cond ] );
        };
        '(dolist (' ~ $.topic.emit ~ ' ' ~ $cond.emit ~ ') ' ~ $block.emit ~ ')';
    }
}

class Decl {
    has $.decl;
    has $.type;
    has $.var;
    method emit {
        my $decl := $.decl;
        my $name := $.var.name;
           ( $decl eq 'has' )
        ?? ( 'sub ' ~ $name ~ ' { ' ~
            '@_ == 1 ' ~
                '? ( $_[0]->{' ~ $name ~ '} ) ' ~
                ': ( $_[0]->{' ~ $name ~ '} = $_[1] ) ' ~
            '}' )
        !! $.decl ~ ' ' ~ $.type ~ ' ' ~ $.var.emit;
    }
}

class Sig {
    has $.invocant;
    has $.positional;
    has $.named;
    method emit {
        ' print \'Signature - TODO\'; die \'Signature - TODO\'; '
    };
    method invocant {
        $.invocant
    };
    method positional {
        $.positional
    }
}

class Method {
    has $.name;
    has $.sig;
    has @.block;
    method emit {
        # unused
    }
}

class Sub {
    has $.name;
    has $.sig;
    has @.block;
    method emit {
        my $sig := $.sig;
        my $pos := $sig.positional;
        my $block := ::MiniPerl6::Lisp::LexicalBlock( block => @.block );
        my $str;
        for @$pos -> $field { 
            $str := $str ~ $field.emit ~ ' ';
        };
        if $str {
            $str := '&optional ' ~ $str;
        }

        if $.name {
            '(defun ' ~ Main::to_lisp_identifier($.name) ~ ' (' ~ $str ~ ')' ~ Main.newline 
                ~ '  (block mp6-function ' ~ $block.emit 
            ~ '))' ~ Main.newline;
        }
        else {
            '(lambda ' ~ $.name ~ ' (' ~ $str ~ ')' ~ Main.newline 
                ~ '  (block mp6-function ' ~ $block.emit 
            ~ '))' ~ Main.newline;

        }

    }
}

class Do {
    has @.block;
    method emit {
        my $block := ::MiniPerl6::Lisp::LexicalBlock( block => @.block );
        return $block.emit;
    }
}

class Use {
    has $.mod;
    method emit {
        Main.newline
        ~ ';; use ' ~ Main::to_lisp_namespace($.mod) ~ Main.newline
    }
}

=begin

=head1 NAME 

MiniPerl6::Lisp::Emit - Code generator for MiniPerl6-in-Lisp (SBCL)

=head1 SYNOPSIS

    $program.emit  # generated Lisp code

=head1 DESCRIPTION

This module generates Lisp code for the MiniPerl6 compiler.

=head1 AUTHORS

Flavio Soibelmann Glock <fglock@gmail.com>.
The Pugs Team E<lt>perl6-compiler@perl.orgE<gt>.

=head1 SEE ALSO

The Perl 6 homepage at L<http://dev.perl.org/perl6>.

The Pugs homepage at L<http://pugscode.org/>.

=head1 COPYRIGHT

Copyright 2009 by Flavio Soibelmann Glock.

This program is free software; you can redistribute it and/or modify it
under the same terms as Perl itself.

See L<http://www.perl.com/perl/misc/Artistic.html>

=end
