# Emulate context vars by localizing 'our' vars living in main
our $CTX;

our $moreinput;
our $DEBUG;
our $DELIM;

our %INSTANTIATED;
require 'mangle.pl';

package sm0p;
use strict;
use warnings;
no warnings 'qw', 'recursion';
use Cursor; # for base class as well as DEBUG constants
use Moose ':all' => { -prefix => "moose_" };
moose_extends('Cursor');
my $retree;
use feature 'state', 'say';
use utf8;

$DB::deep = $DB::deep = 1000; # suppress used-once warning

sub BUILD {
    my $self = shift;
}

use YAML::XS;
#{
#    local $/;
#    my $yaml = <DATA>;
#    *::RE = Load($yaml);
#}

## method lineof ($p)
sub lineof {
    my $self = shift;
    my $p = shift;


    return 1 unless defined $p;
    my $posprops = $self->{'_'};
    my $line = $posprops->[$p]{'line'};
    return $line if $line;
    $line = 1;
    my $pos = 0;
    my $orig = $self->orig;
    my $text = $$orig;
    while ($text ne '') { # XXX needs to recognize #line?
        $posprops->[$pos++]{'line'} = $line;
        $line++ if substr($text,0,1,'') eq "\n";
    }
    return $posprops->[$p]{'line'} // 0;
}
## token TOP
##      token TOP {
##          <ws> <name>  <ws> '=' <ws> 'q:sm0p' <frame> <ws> ';'?
##          {*}  #= sm0p_0
##      
##      }

sub TOP {
    my $self = shift;
    local $CTX = $self->callm() if $::DEBUG & DEBUG::trace_call;
    if ($self->{_peek}) {
        return $self->_AUTOLEXpeek('TOP',$retree)
    }


    my $C = $self;
    $C->{'ws'} = [];

    $self->_MATCHIFY(
        Cursor::lazymap(sub { my $C=$_[0];
            Cursor::lazymap(sub { my $C=$_[0];
                Cursor::lazymap(sub { my $C=$_[0];
                    Cursor::lazymap(sub { my $C=$_[0];
                        Cursor::lazymap(sub { my $C=$_[0];
                            Cursor::lazymap(sub { my $C=$_[0];
                                Cursor::lazymap(sub { my $C=$_[0];
                                    Cursor::lazymap(sub { my $C=$_[0];
                                        Cursor::lazymap(sub { my $C=$_[0];
                                            $C->_REDUCE('TOP sm0p_0')
                                        }, $C->_OPTr(sub { my $C=shift;
                                            $C->_EXACT(';')
                                        }))
                                    }, $C->_SUBSUME(['ws'], sub {
                                        my $C = shift()->cursor_fresh;
                                        $C->ws
                                    }))
                                }, $C->_SUBSUME(['frame'], sub {
                                    my $C = shift()->cursor_fresh;
                                    $C->frame
                                }))
                            }, $C->_EXACT('q:sm0p'))
                        }, $C->_SUBSUME(['ws'], sub {
                            my $C = shift()->cursor_fresh;
                            $C->ws
                        }))
                    }, $C->_EXACT('='))
                }, $C->_SUBSUME(['ws'], sub {
                    my $C = shift()->cursor_fresh;
                    $C->ws
                }))
            }, $C->_SUBSUME(['name'], sub {
                my $C = shift()->cursor_fresh;
                $C->name
            }))
        }, $C->_SUBSUME(['ws'], sub {
            my $C = shift()->cursor_fresh;
            $C->ws
        }))
    );
}
## token frame
##      token frame {
##          <ws> '{' <ws> <nodes> <ws> '}' <ws>
##          {*}  #= sm0p_1
##      
##      }

sub frame {
    my $self = shift;
    local $CTX = $self->callm() if $::DEBUG & DEBUG::trace_call;
    if ($self->{_peek}) {
        return $self->_AUTOLEXpeek('frame',$retree)
    }


    my $C = $self;
    $C->{'ws'} = [];

    $self->_MATCHIFY(
        Cursor::lazymap(sub { my $C=$_[0];
            Cursor::lazymap(sub { my $C=$_[0];
                Cursor::lazymap(sub { my $C=$_[0];
                    Cursor::lazymap(sub { my $C=$_[0];
                        Cursor::lazymap(sub { my $C=$_[0];
                            Cursor::lazymap(sub { my $C=$_[0];
                                Cursor::lazymap(sub { my $C=$_[0];
                                    $C->_REDUCE('frame sm0p_1')
                                }, $C->_SUBSUME(['ws'], sub {
                                    my $C = shift()->cursor_fresh;
                                    $C->ws
                                }))
                            }, $C->_EXACT('}'))
                        }, $C->_SUBSUME(['ws'], sub {
                            my $C = shift()->cursor_fresh;
                            $C->ws
                        }))
                    }, $C->_SUBSUME(['nodes'], sub {
                        my $C = shift()->cursor_fresh;
                        $C->nodes
                    }))
                }, $C->_SUBSUME(['ws'], sub {
                    my $C = shift()->cursor_fresh;
                    $C->ws
                }))
            }, $C->_EXACT('{'))
        }, $C->_SUBSUME(['ws'], sub {
            my $C = shift()->cursor_fresh;
            $C->ws
        }))
    );
}

## token nodes
##      token nodes {
##          {*}  #= sm0p_2
##      
##          <node>*
##          {*}  #= sm0p_3
##      
##          {*}  #= sm0p_4
##      
##      }

sub nodes {
    my $self = shift;
    local $CTX = $self->callm() if $::DEBUG & DEBUG::trace_call;
    if ($self->{_peek}) {
        return $self->_AUTOLEXpeek('nodes',$retree)
    }


    my $C = $self;
    $C->{'node'} = [];

    $self->_MATCHIFY(
        Cursor::lazymap(sub { my $C=$_[0];
            Cursor::lazymap(sub { my $C=$_[0];
                Cursor::lazymap(sub { my $C=$_[0];
                    $C->_REDUCE('nodes sm0p_4')
                }, $C->_REDUCE('nodes sm0p_3'))
            }, $C->_STARr(sub { my $C=shift;
                $C->_SUBSUME(['node'], sub {
                    my $C = shift()->cursor_fresh;
                    $C->node
                })
            }))
        }, $C->_REDUCE('nodes sm0p_2'))
    );
}

## token label
##      token label {
##          <ws> <name> ':' <ws>
##          {*}  #= sm0p_5
##      
##      }

sub label {
    my $self = shift;
    local $CTX = $self->callm() if $::DEBUG & DEBUG::trace_call;
    if ($self->{_peek}) {
        return $self->_AUTOLEXpeek('label',$retree)
    }


    my $C = $self;
    $C->{'ws'} = [];

    $self->_MATCHIFY(
        Cursor::lazymap(sub { my $C=$_[0];
            Cursor::lazymap(sub { my $C=$_[0];
                Cursor::lazymap(sub { my $C=$_[0];
                    Cursor::lazymap(sub { my $C=$_[0];
                        $C->_REDUCE('label sm0p_5')
                    }, $C->_SUBSUME(['ws'], sub {
                        my $C = shift()->cursor_fresh;
                        $C->ws
                    }))
                }, $C->_EXACT(':'))
            }, $C->_SUBSUME(['name'], sub {
                my $C = shift()->cursor_fresh;
                $C->name
            }))
        }, $C->_SUBSUME(['ws'], sub {
            my $C = shift()->cursor_fresh;
            $C->ws
        }))
    );
}
## token node
##      token node {
##          <label>?
##          [<node_empty> {*}  #= sm0p_6
##      
##          ||<node_result> {*}  #= sm0p_7
##      
##          ||<node_capturized> {*}  #= sm0p_8
##      
##          ||<node_full> {*}  #= sm0p_9
##      ]
##          {*}  #= sm0p_10
##      
##      }

sub node {
    my $self = shift;
    local $CTX = $self->callm() if $::DEBUG & DEBUG::trace_call;
    if ($self->{_peek}) {
        return $self->_AUTOLEXpeek('node',$retree)
    }


    my $C = $self;
    $C->{'label'} = [];

    $self->_MATCHIFY(
        Cursor::lazymap(sub { my $C=$_[0];
            Cursor::lazymap(sub { my $C=$_[0];
                $C->_REDUCE('node sm0p_10')
            }, $C->_BRACKET( sub { my $C=shift;
                    do { my @gather;
                            eval { push @gather, Cursor::lazymap(sub { my $C=$_[0];
                                $C->_REDUCE('node sm0p_6')
                            }, $C->_SUBSUME(['node_empty'], sub {
                                my $C = shift()->cursor_fresh;
                                $C->node_empty
                            }))} 
                            or
                            eval { push @gather, Cursor::lazymap(sub { my $C=$_[0];
                                $C->_REDUCE('node sm0p_7')
                            }, $C->_SUBSUME(['node_result'], sub {
                                my $C = shift()->cursor_fresh;
                                $C->node_result
                            }))} 
                            or
                            eval { push @gather, Cursor::lazymap(sub { my $C=$_[0];
                                $C->_REDUCE('node sm0p_8')
                            }, $C->_SUBSUME(['node_capturized'], sub {
                                my $C = shift()->cursor_fresh;
                                $C->node_capturized
                            }))} 
                            or
                            eval { push @gather, Cursor::lazymap(sub { my $C=$_[0];
                                $C->_REDUCE('node sm0p_9')
                            }, $C->_SUBSUME(['node_full'], sub {
                                my $C = shift()->cursor_fresh;
                                $C->node_full
                            }))} or do { die $@ if $@ };
                      @gather;
                    }
            }))
        }, $C->_OPTr(sub { my $C=shift;
            $C->_SUBSUME(['label'], sub {
                my $C = shift()->cursor_fresh;
                $C->label
            })
        }))
    );
}

## token node_empty
##      token node_empty {
##           <ws>
##        ';'
##          {*}  #= sm0p_11
##      
##      }

sub node_empty {
    my $self = shift;
    local $CTX = $self->callm() if $::DEBUG & DEBUG::trace_call;
    if ($self->{_peek}) {
        return $self->_AUTOLEXpeek('node_empty',$retree)
    }


    my $C = $self;

    $self->_MATCHIFY(
        Cursor::lazymap(sub { my $C=$_[0];
            Cursor::lazymap(sub { my $C=$_[0];
                $C->_REDUCE('node_empty sm0p_11')
            }, $C->_EXACT(';'))
        }, $C->_SUBSUME(['ws'], sub {
            my $C = shift()->cursor_fresh;
            $C->ws
        }))
    );
}

## token node_full
##      token node_full {
##          <ws> <responder> '.' <identifier> '('
##          [ <invocant>||'' ] <ws> <named> <ws> <positional> <ws> ')' <ws> ';' <ws>
##          {*}  #= sm0p_12
##      
##      }

sub node_full {
    my $self = shift;
    local $CTX = $self->callm() if $::DEBUG & DEBUG::trace_call;
    if ($self->{_peek}) {
        return $self->_AUTOLEXpeek('node_full',$retree)
    }


    my $C = $self;
    $C->{'ws'} = [];

    $self->_MATCHIFY(
        Cursor::lazymap(sub { my $C=$_[0];
            Cursor::lazymap(sub { my $C=$_[0];
                Cursor::lazymap(sub { my $C=$_[0];
                    Cursor::lazymap(sub { my $C=$_[0];
                        Cursor::lazymap(sub { my $C=$_[0];
                            Cursor::lazymap(sub { my $C=$_[0];
                                Cursor::lazymap(sub { my $C=$_[0];
                                    Cursor::lazymap(sub { my $C=$_[0];
                                        Cursor::lazymap(sub { my $C=$_[0];
                                            Cursor::lazymap(sub { my $C=$_[0];
                                                Cursor::lazymap(sub { my $C=$_[0];
                                                    Cursor::lazymap(sub { my $C=$_[0];
                                                        Cursor::lazymap(sub { my $C=$_[0];
                                                            Cursor::lazymap(sub { my $C=$_[0];
                                                                Cursor::lazymap(sub { my $C=$_[0];
                                                                    $C->_REDUCE('node_full sm0p_12')
                                                                }, $C->_SUBSUME(['ws'], sub {
                                                                    my $C = shift()->cursor_fresh;
                                                                    $C->ws
                                                                }))
                                                            }, $C->_EXACT(';'))
                                                        }, $C->_SUBSUME(['ws'], sub {
                                                            my $C = shift()->cursor_fresh;
                                                            $C->ws
                                                        }))
                                                    }, $C->_EXACT(')'))
                                                }, $C->_SUBSUME(['ws'], sub {
                                                    my $C = shift()->cursor_fresh;
                                                    $C->ws
                                                }))
                                            }, $C->_SUBSUME(['positional'], sub {
                                                my $C = shift()->cursor_fresh;
                                                $C->positional
                                            }))
                                        }, $C->_SUBSUME(['ws'], sub {
                                            my $C = shift()->cursor_fresh;
                                            $C->ws
                                        }))
                                    }, $C->_SUBSUME(['named'], sub {
                                        my $C = shift()->cursor_fresh;
                                        $C->named
                                    }))
                                }, $C->_SUBSUME(['ws'], sub {
                                    my $C = shift()->cursor_fresh;
                                    $C->ws
                                }))
                            }, $C->_BRACKET( sub { my $C=shift;
                                    do { my @gather;
                                            eval { push @gather, $C->_SUBSUME(['invocant'], sub {
                                                my $C = shift()->cursor_fresh;
                                                $C->invocant
                                            })} 
                                            or
                                            eval { push @gather, $C->_EXACT('')} or do { die $@ if $@ };
                                      @gather;
                                    }
                            }))
                        }, $C->_EXACT('('))
                    }, $C->_SUBSUME(['identifier'], sub {
                        my $C = shift()->cursor_fresh;
                        $C->identifier
                    }))
                }, $C->_EXACT('.'))
            }, $C->_SUBSUME(['responder'], sub {
                my $C = shift()->cursor_fresh;
                $C->responder
            }))
        }, $C->_SUBSUME(['ws'], sub {
            my $C = shift()->cursor_fresh;
            $C->ws
        }))
    );
}

## token node_capturized
##      token node_capturized {
##          <ws> <responder> '.' <identifier> '('
##          <ws> '|' <identifier2> <ws> ')' <ws> ';' <ws>
##          {*}  #= sm0p_13
##      
##      }

sub node_capturized {
    my $self = shift;
    local $CTX = $self->callm() if $::DEBUG & DEBUG::trace_call;
    if ($self->{_peek}) {
        return $self->_AUTOLEXpeek('node_capturized',$retree)
    }


    my $C = $self;
    $C->{'ws'} = [];

    $self->_MATCHIFY(
        Cursor::lazymap(sub { my $C=$_[0];
            Cursor::lazymap(sub { my $C=$_[0];
                Cursor::lazymap(sub { my $C=$_[0];
                    Cursor::lazymap(sub { my $C=$_[0];
                        Cursor::lazymap(sub { my $C=$_[0];
                            Cursor::lazymap(sub { my $C=$_[0];
                                Cursor::lazymap(sub { my $C=$_[0];
                                    Cursor::lazymap(sub { my $C=$_[0];
                                        Cursor::lazymap(sub { my $C=$_[0];
                                            Cursor::lazymap(sub { my $C=$_[0];
                                                Cursor::lazymap(sub { my $C=$_[0];
                                                    Cursor::lazymap(sub { my $C=$_[0];
                                                        Cursor::lazymap(sub { my $C=$_[0];
                                                            $C->_REDUCE('node_capturized sm0p_13')
                                                        }, $C->_SUBSUME(['ws'], sub {
                                                            my $C = shift()->cursor_fresh;
                                                            $C->ws
                                                        }))
                                                    }, $C->_EXACT(';'))
                                                }, $C->_SUBSUME(['ws'], sub {
                                                    my $C = shift()->cursor_fresh;
                                                    $C->ws
                                                }))
                                            }, $C->_EXACT(')'))
                                        }, $C->_SUBSUME(['ws'], sub {
                                            my $C = shift()->cursor_fresh;
                                            $C->ws
                                        }))
                                    }, $C->_SUBSUME(['identifier2'], sub {
                                        my $C = shift()->cursor_fresh;
                                        $C->identifier2
                                    }))
                                }, $C->_EXACT('|'))
                            }, $C->_SUBSUME(['ws'], sub {
                                my $C = shift()->cursor_fresh;
                                $C->ws
                            }))
                        }, $C->_EXACT('('))
                    }, $C->_SUBSUME(['identifier'], sub {
                        my $C = shift()->cursor_fresh;
                        $C->identifier
                    }))
                }, $C->_EXACT('.'))
            }, $C->_SUBSUME(['responder'], sub {
                my $C = shift()->cursor_fresh;
                $C->responder
            }))
        }, $C->_SUBSUME(['ws'], sub {
            my $C = shift()->cursor_fresh;
            $C->ws
        }))
    );
}

## token node_result
##      token node_result {
##          <ws> <value> <ws> ';'
##          {*}  #= sm0p_14
##      
##      }

sub node_result {
    my $self = shift;
    local $CTX = $self->callm() if $::DEBUG & DEBUG::trace_call;
    if ($self->{_peek}) {
        return $self->_AUTOLEXpeek('node_result',$retree)
    }


    my $C = $self;
    $C->{'ws'} = [];

    $self->_MATCHIFY(
        Cursor::lazymap(sub { my $C=$_[0];
            Cursor::lazymap(sub { my $C=$_[0];
                Cursor::lazymap(sub { my $C=$_[0];
                    Cursor::lazymap(sub { my $C=$_[0];
                        $C->_REDUCE('node_result sm0p_14')
                    }, $C->_EXACT(';'))
                }, $C->_SUBSUME(['ws'], sub {
                    my $C = shift()->cursor_fresh;
                    $C->ws
                }))
            }, $C->_SUBSUME(['value'], sub {
                my $C = shift()->cursor_fresh;
                $C->value
            }))
        }, $C->_SUBSUME(['ws'], sub {
            my $C = shift()->cursor_fresh;
            $C->ws
        }))
    );
}

## token invocant
##      token invocant {
##          <ws> <identifier> <ws> ':' <ws>
##          {*}  #= sm0p_15
##      
##      }

sub invocant {
    my $self = shift;
    local $CTX = $self->callm() if $::DEBUG & DEBUG::trace_call;
    if ($self->{_peek}) {
        return $self->_AUTOLEXpeek('invocant',$retree)
    }


    my $C = $self;
    $C->{'ws'} = [];

    $self->_MATCHIFY(
        Cursor::lazymap(sub { my $C=$_[0];
            Cursor::lazymap(sub { my $C=$_[0];
                Cursor::lazymap(sub { my $C=$_[0];
                    Cursor::lazymap(sub { my $C=$_[0];
                        Cursor::lazymap(sub { my $C=$_[0];
                            $C->_REDUCE('invocant sm0p_15')
                        }, $C->_SUBSUME(['ws'], sub {
                            my $C = shift()->cursor_fresh;
                            $C->ws
                        }))
                    }, $C->_EXACT(':'))
                }, $C->_SUBSUME(['ws'], sub {
                    my $C = shift()->cursor_fresh;
                    $C->ws
                }))
            }, $C->_SUBSUME(['identifier'], sub {
                my $C = shift()->cursor_fresh;
                $C->identifier
            }))
        }, $C->_SUBSUME(['ws'], sub {
            my $C = shift()->cursor_fresh;
            $C->ws
        }))
    );
}

## token responder
##      token responder {
##          <identifier>
##          {*}  #= sm0p_16
##      
##      }

sub responder {
    my $self = shift;
    local $CTX = $self->callm() if $::DEBUG & DEBUG::trace_call;
    if ($self->{_peek}) {
        return $self->_AUTOLEXpeek('responder',$retree)
    }


    my $C = $self;

    $self->_MATCHIFY(
        Cursor::lazymap(sub { my $C=$_[0];
            $C->_REDUCE('responder sm0p_16')
        }, $C->_SUBSUME(['identifier'], sub {
            my $C = shift()->cursor_fresh;
            $C->identifier
        }))
    );
}

## token positional
##      token positional {
##          <positionals>
##          {*}  #= sm0p_17
##      
##       ||{*}  #= sm0p_18
##      
##      }

sub positional {
    my $self = shift;
    local $CTX = $self->callm() if $::DEBUG & DEBUG::trace_call;
    if ($self->{_peek}) {
        return $self->_AUTOLEXpeek('positional',$retree)
    }


    my $C = $self;

    $self->_MATCHIFY(
        do { my @gather;
                eval { push @gather, Cursor::lazymap(sub { my $C=$_[0];
                    $C->_REDUCE('positional sm0p_17')
                }, $C->_SUBSUME(['positionals'], sub {
                    my $C = shift()->cursor_fresh;
                    $C->positionals
                }))} 
                or
                eval { push @gather, $C->_REDUCE('positional sm0p_18')} or do { die $@ if $@ };
          @gather;
        }
    );
}

# workaround
## token notquote_or_backslashed
##      token notquote_or_backslashed {
##          '\\' . || <-["]>
##      }

sub notquote_or_backslashed {
    my $self = shift;
    local $CTX = $self->callm() if $::DEBUG & DEBUG::trace_call;
    if ($self->{_peek}) {
        return $self->_AUTOLEXpeek('notquote_or_backslashed',$retree)
    }


    my $C = $self;

    $self->_MATCHIFY(
        do { my @gather;
                eval { push @gather, Cursor::lazymap(sub { my $C=$_[0];
                    $C->_ANY()
                }, $C->_EXACT('\\'))} 
                or
                eval { push @gather, $C->_CCLASS(qr/^[^"]$/)} or do { die $@ if $@ };
          @gather;
        }
    );
}
## token nativestring
##      token nativestring {
##          '"' (<.notquote_or_backslashed>*) '"'
##          {*}  #= sm0p_19
##      
##      }

sub nativestring {
    my $self = shift;
    local $CTX = $self->callm() if $::DEBUG & DEBUG::trace_call;
    if ($self->{_peek}) {
        return $self->_AUTOLEXpeek('nativestring',$retree)
    }


    my $C = $self;

    $self->_MATCHIFY(
        Cursor::lazymap(sub { my $C=$_[0];
            Cursor::lazymap(sub { my $C=$_[0];
                Cursor::lazymap(sub { my $C=$_[0];
                    $C->_REDUCE('nativestring sm0p_19')
                }, $C->_EXACT('"'))
            },     $C->_SUBSUME(['0'], sub {
                    my $C = shift()->cursor_fresh;
                    do { $C->_PAREN( sub { my $C=shift;
                            $C->_STARr(sub { my $C=shift;
                                $C->notquote_or_backslashed
                            })
                    })}
                }))
        }, $C->_EXACT('"'))
    );
}

## token value
##      token value {
##          || <frame>    {*}  #= sm0p_20
##      
##          || <nativestring> {*}  #= sm0p_21
##      
##          || <nativeint>    {*}  #= sm0p_22
##      
##          || <capturize>    {*}  #= sm0p_23
##      
##          || <identifier>   {*}  #= sm0p_24
##      
##          || "`" <name>     {*}  #= sm0p_25
##      
##      }

sub value {
    my $self = shift;
    local $CTX = $self->callm() if $::DEBUG & DEBUG::trace_call;
    if ($self->{_peek}) {
        return $self->_AUTOLEXpeek('value',$retree)
    }


    my $C = $self;

    $self->_MATCHIFY(
        do { my @gather;
                eval { push @gather, Cursor::lazymap(sub { my $C=$_[0];
                    $C->_REDUCE('value sm0p_20')
                }, $C->_SUBSUME(['frame'], sub {
                    my $C = shift()->cursor_fresh;
                    $C->frame
                }))} 
                or
                eval { push @gather, Cursor::lazymap(sub { my $C=$_[0];
                    $C->_REDUCE('value sm0p_21')
                }, $C->_SUBSUME(['nativestring'], sub {
                    my $C = shift()->cursor_fresh;
                    $C->nativestring
                }))} 
                or
                eval { push @gather, Cursor::lazymap(sub { my $C=$_[0];
                    $C->_REDUCE('value sm0p_22')
                }, $C->_SUBSUME(['nativeint'], sub {
                    my $C = shift()->cursor_fresh;
                    $C->nativeint
                }))} 
                or
                eval { push @gather, Cursor::lazymap(sub { my $C=$_[0];
                    $C->_REDUCE('value sm0p_23')
                }, $C->_SUBSUME(['capturize'], sub {
                    my $C = shift()->cursor_fresh;
                    $C->capturize
                }))} 
                or
                eval { push @gather, Cursor::lazymap(sub { my $C=$_[0];
                    $C->_REDUCE('value sm0p_24')
                }, $C->_SUBSUME(['identifier'], sub {
                    my $C = shift()->cursor_fresh;
                    $C->identifier
                }))} 
                or
                eval { push @gather, Cursor::lazymap(sub { my $C=$_[0];
                    Cursor::lazymap(sub { my $C=$_[0];
                        $C->_REDUCE('value sm0p_25')
                    }, $C->_SUBSUME(['name'], sub {
                        my $C = shift()->cursor_fresh;
                        $C->name
                    }))
                }, $C->_EXACT("`"))} or do { die $@ if $@ };
          @gather;
        }
    );
}

## token positionals
##      token positionals {
##        <value>
##          [
##         ||  <ws> \, <ws> <positionals>
##              {*}  #= sm0p_26
##      
##         ||  <ws>
##              {*}  #= sm0p_27
##      
##          ]
##      }

sub positionals {
    my $self = shift;
    local $CTX = $self->callm() if $::DEBUG & DEBUG::trace_call;
    if ($self->{_peek}) {
        return $self->_AUTOLEXpeek('positionals',$retree)
    }


    my $C = $self;
    $C->{'ws'} = [];

    $self->_MATCHIFY(
        Cursor::lazymap(sub { my $C=$_[0];
            $C->_BRACKET( sub { my $C=shift;
                    do { my @gather;
                            eval { push @gather, Cursor::lazymap(sub { my $C=$_[0];
                                Cursor::lazymap(sub { my $C=$_[0];
                                    Cursor::lazymap(sub { my $C=$_[0];
                                        Cursor::lazymap(sub { my $C=$_[0];
                                            $C->_REDUCE('positionals sm0p_26')
                                        }, $C->_SUBSUME(['positionals'], sub {
                                            my $C = shift()->cursor_fresh;
                                            $C->positionals
                                        }))
                                    }, $C->_SUBSUME(['ws'], sub {
                                        my $C = shift()->cursor_fresh;
                                        $C->ws
                                    }))
                                }, $C->_EXACT(','))
                            }, $C->_SUBSUME(['ws'], sub {
                                my $C = shift()->cursor_fresh;
                                $C->ws
                            }))} 
                            or
                            eval { push @gather, Cursor::lazymap(sub { my $C=$_[0];
                                $C->_REDUCE('positionals sm0p_27')
                            }, $C->_SUBSUME(['ws'], sub {
                                my $C = shift()->cursor_fresh;
                                $C->ws
                            }))} or do { die $@ if $@ };
                      @gather;
                    }
            })
        }, $C->_SUBSUME(['value'], sub {
            my $C = shift()->cursor_fresh;
            $C->value
        }))
    );
}

## token named
##      token named {
##          <pairs>
##          {*}  #= sm0p_28
##      
##        ||{*}  #= sm0p_29
##      
##      }

sub named {
    my $self = shift;
    local $CTX = $self->callm() if $::DEBUG & DEBUG::trace_call;
    if ($self->{_peek}) {
        return $self->_AUTOLEXpeek('named',$retree)
    }


    my $C = $self;

    $self->_MATCHIFY(
        do { my @gather;
                eval { push @gather, Cursor::lazymap(sub { my $C=$_[0];
                    $C->_REDUCE('named sm0p_28')
                }, $C->_SUBSUME(['pairs'], sub {
                    my $C = shift()->cursor_fresh;
                    $C->pairs
                }))} 
                or
                eval { push @gather, $C->_REDUCE('named sm0p_29')} or do { die $@ if $@ };
          @gather;
        }
    );
}

## token pairs
##      token pairs {
##          <pair>
##          [
##              <ws> \, <ws> <pairs>
##              {*}  #= sm0p_30
##      
##         ||  <ws> [ \, <ws> || '' ]
##              {*}  #= sm0p_31
##      
##          ]
##      }

sub pairs {
    my $self = shift;
    local $CTX = $self->callm() if $::DEBUG & DEBUG::trace_call;
    if ($self->{_peek}) {
        return $self->_AUTOLEXpeek('pairs',$retree)
    }


    my $C = $self;
    $C->{'ws'} = [];

    $self->_MATCHIFY(
        Cursor::lazymap(sub { my $C=$_[0];
            $C->_BRACKET( sub { my $C=shift;
                    do { my @gather;
                            eval { push @gather, Cursor::lazymap(sub { my $C=$_[0];
                                Cursor::lazymap(sub { my $C=$_[0];
                                    Cursor::lazymap(sub { my $C=$_[0];
                                        Cursor::lazymap(sub { my $C=$_[0];
                                            $C->_REDUCE('pairs sm0p_30')
                                        }, $C->_SUBSUME(['pairs'], sub {
                                            my $C = shift()->cursor_fresh;
                                            $C->pairs
                                        }))
                                    }, $C->_SUBSUME(['ws'], sub {
                                        my $C = shift()->cursor_fresh;
                                        $C->ws
                                    }))
                                }, $C->_EXACT(','))
                            }, $C->_SUBSUME(['ws'], sub {
                                my $C = shift()->cursor_fresh;
                                $C->ws
                            }))} 
                            or
                            eval { push @gather, Cursor::lazymap(sub { my $C=$_[0];
                                Cursor::lazymap(sub { my $C=$_[0];
                                    $C->_REDUCE('pairs sm0p_31')
                                }, $C->_BRACKET( sub { my $C=shift;
                                        do { my @gather;
                                                eval { push @gather, Cursor::lazymap(sub { my $C=$_[0];
                                                    $C->_SUBSUME(['ws'], sub {
                                                        my $C = shift()->cursor_fresh;
                                                        $C->ws
                                                    })
                                                }, $C->_EXACT(','))} 
                                                or
                                                eval { push @gather, $C->_EXACT('')} or do { die $@ if $@ };
                                          @gather;
                                        }
                                }))
                            }, $C->_SUBSUME(['ws'], sub {
                                my $C = shift()->cursor_fresh;
                                $C->ws
                            }))} or do { die $@ if $@ };
                      @gather;
                    }
            })
        }, $C->_SUBSUME(['pair'], sub {
            my $C = shift()->cursor_fresh;
            $C->pair
        }))
    );
}

## token pair
##      token pair {
##          <identifier> <ws> '=>' <ws> <identifier2>
##          {*}  #= sm0p_32
##      
##      }

sub pair {
    my $self = shift;
    local $CTX = $self->callm() if $::DEBUG & DEBUG::trace_call;
    if ($self->{_peek}) {
        return $self->_AUTOLEXpeek('pair',$retree)
    }


    my $C = $self;
    $C->{'ws'} = [];

    $self->_MATCHIFY(
        Cursor::lazymap(sub { my $C=$_[0];
            Cursor::lazymap(sub { my $C=$_[0];
                Cursor::lazymap(sub { my $C=$_[0];
                    Cursor::lazymap(sub { my $C=$_[0];
                        Cursor::lazymap(sub { my $C=$_[0];
                            $C->_REDUCE('pair sm0p_32')
                        }, $C->_SUBSUME(['identifier2'], sub {
                            my $C = shift()->cursor_fresh;
                            $C->identifier2
                        }))
                    }, $C->_SUBSUME(['ws'], sub {
                        my $C = shift()->cursor_fresh;
                        $C->ws
                    }))
                }, $C->_EXACT('=>'))
            }, $C->_SUBSUME(['ws'], sub {
                my $C = shift()->cursor_fresh;
                $C->ws
            }))
        }, $C->_SUBSUME(['identifier'], sub {
            my $C = shift()->cursor_fresh;
            $C->identifier
        }))
    );
}

## token identifier2
##      token identifier2 {
##          <identifier> {*}  #= sm0p_33
##      
##      }

sub identifier2 {
    my $self = shift;
    local $CTX = $self->callm() if $::DEBUG & DEBUG::trace_call;
    if ($self->{_peek}) {
        return $self->_AUTOLEXpeek('identifier2',$retree)
    }


    my $C = $self;

    $self->_MATCHIFY(
        Cursor::lazymap(sub { my $C=$_[0];
            $C->_REDUCE('identifier2 sm0p_33')
        }, $C->_SUBSUME(['identifier'], sub {
            my $C = shift()->cursor_fresh;
            $C->identifier
        }))
    );
}


## token notbracket_or_backslashed
##      token notbracket_or_backslashed {
##          '\\' . || <-[\]]>
##      }

sub notbracket_or_backslashed {
    my $self = shift;
    local $CTX = $self->callm() if $::DEBUG & DEBUG::trace_call;
    if ($self->{_peek}) {
        return $self->_AUTOLEXpeek('notbracket_or_backslashed',$retree)
    }


    my $C = $self;

    $self->_MATCHIFY(
        do { my @gather;
                eval { push @gather, Cursor::lazymap(sub { my $C=$_[0];
                    $C->_ANY()
                }, $C->_EXACT('\\'))} 
                or
                eval { push @gather, $C->_CCLASS(qr/^[^\]]$/)} or do { die $@ if $@ };
          @gather;
        }
    );
}

## token identifier
##      token identifier {
##         '$' <name> {*}  #= sm0p_34
##      
##       || 'q:identifier[' (<.notbracket_or_backslashed>*) ']'
##           {*}  #= sm0p_35
##      
##       || <idconst> {*}  #= sm0p_36
##      
##       || <name> {*}  #= sm0p_37
##      
##      }

sub identifier {
    my $self = shift;
    local $CTX = $self->callm() if $::DEBUG & DEBUG::trace_call;
    if ($self->{_peek}) {
        return $self->_AUTOLEXpeek('identifier',$retree)
    }


    my $C = $self;

    $self->_MATCHIFY(
        do { my @gather;
                eval { push @gather, Cursor::lazymap(sub { my $C=$_[0];
                    Cursor::lazymap(sub { my $C=$_[0];
                        $C->_REDUCE('identifier sm0p_34')
                    }, $C->_SUBSUME(['name'], sub {
                        my $C = shift()->cursor_fresh;
                        $C->name
                    }))
                }, $C->_EXACT('$'))} 
                or
                eval { push @gather, Cursor::lazymap(sub { my $C=$_[0];
                    Cursor::lazymap(sub { my $C=$_[0];
                        Cursor::lazymap(sub { my $C=$_[0];
                            $C->_REDUCE('identifier sm0p_35')
                        }, $C->_EXACT(']'))
                    },     $C->_SUBSUME(['0'], sub {
                            my $C = shift()->cursor_fresh;
                            do { $C->_PAREN( sub { my $C=shift;
                                    $C->_STARr(sub { my $C=shift;
                                        $C->notbracket_or_backslashed
                                    })
                            })}
                        }))
                }, $C->_EXACT('q:identifier['))} 
                or
                eval { push @gather, Cursor::lazymap(sub { my $C=$_[0];
                    $C->_REDUCE('identifier sm0p_36')
                }, $C->_SUBSUME(['idconst'], sub {
                    my $C = shift()->cursor_fresh;
                    $C->idconst
                }))} 
                or
                eval { push @gather, Cursor::lazymap(sub { my $C=$_[0];
                    $C->_REDUCE('identifier sm0p_37')
                }, $C->_SUBSUME(['name'], sub {
                    my $C = shift()->cursor_fresh;
                    $C->name
                }))} or do { die $@ if $@ };
          @gather;
        }
    );
}

## token idconst
##      token idconst {
##        <idconst_list> {*}  #= sm0p_38
##      
##      }

sub idconst {
    my $self = shift;
    local $CTX = $self->callm() if $::DEBUG & DEBUG::trace_call;
    if ($self->{_peek}) {
        return $self->_AUTOLEXpeek('idconst',$retree)
    }


    my $C = $self;

    $self->_MATCHIFY(
        Cursor::lazymap(sub { my $C=$_[0];
            $C->_REDUCE('idconst sm0p_38')
        }, $C->_SUBSUME(['idconst_list'], sub {
            my $C = shift()->cursor_fresh;
            $C->idconst_list
        }))
    );
}

## token idconst_list
##      token idconst_list {(new|lexical|back|capture|continuation|continues|copy|current|debug|drop|DESTROYALL|FETCH|STORE|eval|forget|free|goto|has_next|identifier|jail|lexical|loop|move_capturize|move_identifier|move_responder|new|next|past|push|responder|result|setr|outer)}

sub idconst_list {
    my $self = shift;
    local $CTX = $self->callm() if $::DEBUG & DEBUG::trace_call;
    if ($self->{_peek}) {
        return $self->_AUTOLEXpeek('idconst_list',$retree)
    }


    my $C = $self;

    $self->_MATCHIFY(
            $C->_SUBSUME(['0'], sub {
                my $C = shift()->cursor_fresh;
                do { $C->_PAREN( sub { my $C=shift;
                        do {
                          my @result = eval {
                            my $trystate;
                            my ($C, $tag, $try, $relex) = $C->cursor_fate('sm0p', 'idconst_list_01', $retree, $trystate);
                            my @try = $tag eq 'idconst_list_01' ? ($try,$relex) : ();
                        
                            my @gather = ();
                            while (@try and not @gather) {
                                my $try = shift(@try) // next;
                        
                                # if first try failed, interrogate lexer for shorter answers
                                if (ref $try) {
                                    if (ref $try eq 'ARRAY') {
                                        ($tag, $try, $C->{_fate}) = @$try;	# next candidate fate
                                        $C->deb("Retrying with $tag $try") if $::DEBUG & DEBUG::try_processing;
                                    }
                                    elsif (ref $try eq 'CODE') {
                                        my @more = $relex->($C, $trystate);
                                        if (@more) {
                                            unshift @try, @more, $relex;   # ask relex for more
                                        }
                                        next;
                                    }
                                }
                        
                                $C->deb("idconst_list_01 trying $tag $try") if $::DEBUG & DEBUG::try_processing;
                                push @gather, ((
                                    sub { my $C=shift;
                                        $C->_EXACT('new')
                                    },
                                    sub { my $C=shift;
                                        $C->_EXACT('lexical')
                                    },
                                    sub { my $C=shift;
                                        $C->_EXACT('back')
                                    },
                                    sub { my $C=shift;
                                        $C->_EXACT('capture')
                                    },
                                    sub { my $C=shift;
                                        $C->_EXACT('continuation')
                                    },
                                    sub { my $C=shift;
                                        $C->_EXACT('continues')
                                    },
                                    sub { my $C=shift;
                                        $C->_EXACT('copy')
                                    },
                                    sub { my $C=shift;
                                        $C->_EXACT('current')
                                    },
                                    sub { my $C=shift;
                                        $C->_EXACT('debug')
                                    },
                                    sub { my $C=shift;
                                        $C->_EXACT('drop')
                                    },
                                    sub { my $C=shift;
                                        $C->_EXACT('DESTROYALL')
                                    },
                                    sub { my $C=shift;
                                        $C->_EXACT('FETCH')
                                    },
                                    sub { my $C=shift;
                                        $C->_EXACT('STORE')
                                    },
                                    sub { my $C=shift;
                                        $C->_EXACT('eval')
                                    },
                                    sub { my $C=shift;
                                        $C->_EXACT('forget')
                                    },
                                    sub { my $C=shift;
                                        $C->_EXACT('free')
                                    },
                                    sub { my $C=shift;
                                        $C->_EXACT('goto')
                                    },
                                    sub { my $C=shift;
                                        $C->_EXACT('has_next')
                                    },
                                    sub { my $C=shift;
                                        $C->_EXACT('identifier')
                                    },
                                    sub { my $C=shift;
                                        $C->_EXACT('jail')
                                    },
                                    sub { my $C=shift;
                                        $C->_EXACT('lexical')
                                    },
                                    sub { my $C=shift;
                                        $C->_EXACT('loop')
                                    },
                                    sub { my $C=shift;
                                        $C->_EXACT('move_capturize')
                                    },
                                    sub { my $C=shift;
                                        $C->_EXACT('move_identifier')
                                    },
                                    sub { my $C=shift;
                                        $C->_EXACT('move_responder')
                                    },
                                    sub { my $C=shift;
                                        $C->_EXACT('new')
                                    },
                                    sub { my $C=shift;
                                        $C->_EXACT('next')
                                    },
                                    sub { my $C=shift;
                                        $C->_EXACT('past')
                                    },
                                    sub { my $C=shift;
                                        $C->_EXACT('push')
                                    },
                                    sub { my $C=shift;
                                        $C->_EXACT('responder')
                                    },
                                    sub { my $C=shift;
                                        $C->_EXACT('result')
                                    },
                                    sub { my $C=shift;
                                        $C->_EXACT('setr')
                                    },
                                    sub { my $C=shift;
                                        $C->_EXACT('outer')
                                    },
                                )[$try])->($C);
                            }
                            @gather;
                          };
                          if (@result) {
                            @result;
                          }
                          elsif ($@) {
                            my ($first) = $@ =~ /(^.*)/;
                            $self->deb("CAUGHT $first") if $::DEBUG & DEBUG::trace_call;
                            die $@ if $@ !~ /^ABORTBRANCH/;
                            ();
                          }
                          else {
                            ();
                          }
                        }
                
                })}
            })
    );
}


## token ws
##      token ws  {[\s|'#'\N*\n]*}

sub ws {
    my $self = shift;
    local $CTX = $self->callm() if $::DEBUG & DEBUG::trace_call;
    if ($self->{_peek}) {
        return ''
    }


    my $C = $self;

    $self->_MATCHIFY(
        $C->_STARr(sub { my $C=shift;
            $C->_BRACKET( sub { my $C=shift;
                    do {
                      my @result = eval {
                        my $trystate;
                        my ($C, $tag, $try, $relex) = $C->cursor_fate('sm0p', 'ws_01', $retree, $trystate);
                        my @try = $tag eq 'ws_01' ? ($try,$relex) : ();
                    
                        my @gather = ();
                        while (@try and not @gather) {
                            my $try = shift(@try) // next;
                    
                            # if first try failed, interrogate lexer for shorter answers
                            if (ref $try) {
                                if (ref $try eq 'ARRAY') {
                                    ($tag, $try, $C->{_fate}) = @$try;	# next candidate fate
                                    $C->deb("Retrying with $tag $try") if $::DEBUG & DEBUG::try_processing;
                                }
                                elsif (ref $try eq 'CODE') {
                                    my @more = $relex->($C, $trystate);
                                    if (@more) {
                                        unshift @try, @more, $relex;   # ask relex for more
                                    }
                                    next;
                                }
                            }
                    
                            $C->deb("ws_01 trying $tag $try") if $::DEBUG & DEBUG::try_processing;
                            push @gather, ((
                                sub { my $C=shift;
                                    $C->_SPACE()
                                },
                                sub { my $C=shift;
                                    Cursor::lazymap(sub { my $C=$_[0];
                                        Cursor::lazymap(sub { my $C=$_[0];
                                            $C->_EXACT("\n")
                                        }, $C->_STARr(sub { my $C=shift;
                                            $C->_NOTCHAR( sub { my $C=shift;
                                                $C->_EXACT("\n")
                                            })
                                        }))
                                    }, $C->_EXACT('#'))
                                },
                            )[$try])->($C);
                        }
                        @gather;
                      };
                      if (@result) {
                        @result;
                      }
                      elsif ($@) {
                        my ($first) = $@ =~ /(^.*)/;
                        $self->deb("CAUGHT $first") if $::DEBUG & DEBUG::trace_call;
                        die $@ if $@ !~ /^ABORTBRANCH/;
                        ();
                      }
                      else {
                        ();
                      }
                    }
            
            })
        })
    );
} 

## token capturize
##      token capturize {
##          <ws> SMOP__SLIME__Capturize '.'  new  '(' <ws>
##          <cint1> <ws> ',' <ws> <cintlist1> <ws> ',' <ws>
##          <cintlist2> <ws> ',' <ws> <cint2> ')'
##          {*}  #= sm0p_39
##      
##      }

sub capturize {
    my $self = shift;
    local $CTX = $self->callm() if $::DEBUG & DEBUG::trace_call;
    if ($self->{_peek}) {
        return $self->_AUTOLEXpeek('capturize',$retree)
    }


    my $C = $self;
    $C->{'ws'} = [];

    $self->_MATCHIFY(
        Cursor::lazymap(sub { my $C=$_[0];
            Cursor::lazymap(sub { my $C=$_[0];
                Cursor::lazymap(sub { my $C=$_[0];
                    Cursor::lazymap(sub { my $C=$_[0];
                        Cursor::lazymap(sub { my $C=$_[0];
                            Cursor::lazymap(sub { my $C=$_[0];
                                Cursor::lazymap(sub { my $C=$_[0];
                                    Cursor::lazymap(sub { my $C=$_[0];
                                        Cursor::lazymap(sub { my $C=$_[0];
                                            Cursor::lazymap(sub { my $C=$_[0];
                                                Cursor::lazymap(sub { my $C=$_[0];
                                                    Cursor::lazymap(sub { my $C=$_[0];
                                                        Cursor::lazymap(sub { my $C=$_[0];
                                                            Cursor::lazymap(sub { my $C=$_[0];
                                                                Cursor::lazymap(sub { my $C=$_[0];
                                                                    Cursor::lazymap(sub { my $C=$_[0];
                                                                        Cursor::lazymap(sub { my $C=$_[0];
                                                                            Cursor::lazymap(sub { my $C=$_[0];
                                                                                Cursor::lazymap(sub { my $C=$_[0];
                                                                                    Cursor::lazymap(sub { my $C=$_[0];
                                                                                        $C->_REDUCE('capturize sm0p_39')
                                                                                    }, $C->_EXACT(')'))
                                                                                }, $C->_SUBSUME(['cint2'], sub {
                                                                                    my $C = shift()->cursor_fresh;
                                                                                    $C->cint2
                                                                                }))
                                                                            }, $C->_SUBSUME(['ws'], sub {
                                                                                my $C = shift()->cursor_fresh;
                                                                                $C->ws
                                                                            }))
                                                                        }, $C->_EXACT(','))
                                                                    }, $C->_SUBSUME(['ws'], sub {
                                                                        my $C = shift()->cursor_fresh;
                                                                        $C->ws
                                                                    }))
                                                                }, $C->_SUBSUME(['cintlist2'], sub {
                                                                    my $C = shift()->cursor_fresh;
                                                                    $C->cintlist2
                                                                }))
                                                            }, $C->_SUBSUME(['ws'], sub {
                                                                my $C = shift()->cursor_fresh;
                                                                $C->ws
                                                            }))
                                                        }, $C->_EXACT(','))
                                                    }, $C->_SUBSUME(['ws'], sub {
                                                        my $C = shift()->cursor_fresh;
                                                        $C->ws
                                                    }))
                                                }, $C->_SUBSUME(['cintlist1'], sub {
                                                    my $C = shift()->cursor_fresh;
                                                    $C->cintlist1
                                                }))
                                            }, $C->_SUBSUME(['ws'], sub {
                                                my $C = shift()->cursor_fresh;
                                                $C->ws
                                            }))
                                        }, $C->_EXACT(','))
                                    }, $C->_SUBSUME(['ws'], sub {
                                        my $C = shift()->cursor_fresh;
                                        $C->ws
                                    }))
                                }, $C->_SUBSUME(['cint1'], sub {
                                    my $C = shift()->cursor_fresh;
                                    $C->cint1
                                }))
                            }, $C->_SUBSUME(['ws'], sub {
                                my $C = shift()->cursor_fresh;
                                $C->ws
                            }))
                        }, $C->_EXACT('('))
                    }, $C->_EXACT('new'))
                }, $C->_EXACT('.'))
            }, $C->_EXACT('SMOP__SLIME__Capturize'))
        }, $C->_SUBSUME(['ws'], sub {
            my $C = shift()->cursor_fresh;
            $C->ws
        }))
    );
}

## token cint1
##      token cint1 { <cint> {*}  #= sm0p_40
##       }

sub cint1 {
    my $self = shift;
    local $CTX = $self->callm() if $::DEBUG & DEBUG::trace_call;
    if ($self->{_peek}) {
        return $self->_AUTOLEXpeek('cint1',$retree)
    }


    my $C = $self;

    $self->_MATCHIFY(
        Cursor::lazymap(sub { my $C=$_[0];
            $C->_REDUCE('cint1 sm0p_40')
        }, $C->_SUBSUME(['cint'], sub {
            my $C = shift()->cursor_fresh;
            $C->cint
        }))
    );
}
## token cint2
##      token cint2 { <cint> {*}  #= sm0p_41
##       }

sub cint2 {
    my $self = shift;
    local $CTX = $self->callm() if $::DEBUG & DEBUG::trace_call;
    if ($self->{_peek}) {
        return $self->_AUTOLEXpeek('cint2',$retree)
    }


    my $C = $self;

    $self->_MATCHIFY(
        Cursor::lazymap(sub { my $C=$_[0];
            $C->_REDUCE('cint2 sm0p_41')
        }, $C->_SUBSUME(['cint'], sub {
            my $C = shift()->cursor_fresh;
            $C->cint
        }))
    );
}
## token cint
##      token cint {
##          <digits> {*}  #= sm0p_42
##      
##          || "`" <name>   {*}  #= sm0p_43
##      
##      }

sub cint {
    my $self = shift;
    local $CTX = $self->callm() if $::DEBUG & DEBUG::trace_call;
    if ($self->{_peek}) {
        return $self->_AUTOLEXpeek('cint',$retree)
    }


    my $C = $self;

    $self->_MATCHIFY(
        do { my @gather;
                eval { push @gather, Cursor::lazymap(sub { my $C=$_[0];
                    $C->_REDUCE('cint sm0p_42')
                }, $C->_SUBSUME(['digits'], sub {
                    my $C = shift()->cursor_fresh;
                    $C->digits
                }))} 
                or
                eval { push @gather, Cursor::lazymap(sub { my $C=$_[0];
                    Cursor::lazymap(sub { my $C=$_[0];
                        $C->_REDUCE('cint sm0p_43')
                    }, $C->_SUBSUME(['name'], sub {
                        my $C = shift()->cursor_fresh;
                        $C->name
                    }))
                }, $C->_EXACT("`"))} or do { die $@ if $@ };
          @gather;
        }
    );
}

## token cintlist1
##      token cintlist1 { <cintlist> {*}  #= sm0p_44
##       }

sub cintlist1 {
    my $self = shift;
    local $CTX = $self->callm() if $::DEBUG & DEBUG::trace_call;
    if ($self->{_peek}) {
        return $self->_AUTOLEXpeek('cintlist1',$retree)
    }


    my $C = $self;

    $self->_MATCHIFY(
        Cursor::lazymap(sub { my $C=$_[0];
            $C->_REDUCE('cintlist1 sm0p_44')
        }, $C->_SUBSUME(['cintlist'], sub {
            my $C = shift()->cursor_fresh;
            $C->cintlist
        }))
    );
}
## token cintlist2
##      token cintlist2 { <cintlist> {*}  #= sm0p_45
##       }

sub cintlist2 {
    my $self = shift;
    local $CTX = $self->callm() if $::DEBUG & DEBUG::trace_call;
    if ($self->{_peek}) {
        return $self->_AUTOLEXpeek('cintlist2',$retree)
    }


    my $C = $self;

    $self->_MATCHIFY(
        Cursor::lazymap(sub { my $C=$_[0];
            $C->_REDUCE('cintlist2 sm0p_45')
        }, $C->_SUBSUME(['cintlist'], sub {
            my $C = shift()->cursor_fresh;
            $C->cintlist
        }))
    );
}
## token cintlist
##      token cintlist {
##          '('
##          [ <cintlistbody> ')' {*}  #= sm0p_46
##      
##         ||')' {*}  #= sm0p_47
##       ] 
##        ||'' {*}  #= sm0p_48
##      
##      }

sub cintlist {
    my $self = shift;
    local $CTX = $self->callm() if $::DEBUG & DEBUG::trace_call;
    if ($self->{_peek}) {
        return $self->_AUTOLEXpeek('cintlist',$retree)
    }


    my $C = $self;

    $self->_MATCHIFY(
        do { my @gather;
                eval { push @gather, Cursor::lazymap(sub { my $C=$_[0];
                    $C->_BRACKET( sub { my $C=shift;
                            do { my @gather;
                                    eval { push @gather, Cursor::lazymap(sub { my $C=$_[0];
                                        Cursor::lazymap(sub { my $C=$_[0];
                                            $C->_REDUCE('cintlist sm0p_46')
                                        }, $C->_EXACT(')'))
                                    }, $C->_SUBSUME(['cintlistbody'], sub {
                                        my $C = shift()->cursor_fresh;
                                        $C->cintlistbody
                                    }))} 
                                    or
                                    eval { push @gather, Cursor::lazymap(sub { my $C=$_[0];
                                        $C->_REDUCE('cintlist sm0p_47')
                                    }, $C->_EXACT(')'))} or do { die $@ if $@ };
                              @gather;
                            }
                    })
                }, $C->_EXACT('('))} 
                or
                eval { push @gather, Cursor::lazymap(sub { my $C=$_[0];
                    $C->_REDUCE('cintlist sm0p_48')
                }, $C->_EXACT(''))} or do { die $@ if $@ };
          @gather;
        }
    );
}

## token cintlistbody
##      token cintlistbody {
##          <ws> <cint> <ws>
##      [
##           ',' <ws> <cintlistbody> <ws>
##           {*}  #= sm0p_49
##      
##         ||'' {*}  #= sm0p_50
##      
##          ]
##      }

sub cintlistbody {
    my $self = shift;
    local $CTX = $self->callm() if $::DEBUG & DEBUG::trace_call;
    if ($self->{_peek}) {
        return $self->_AUTOLEXpeek('cintlistbody',$retree)
    }


    my $C = $self;
    $C->{'ws'} = [];

    $self->_MATCHIFY(
        Cursor::lazymap(sub { my $C=$_[0];
            Cursor::lazymap(sub { my $C=$_[0];
                Cursor::lazymap(sub { my $C=$_[0];
                    $C->_BRACKET( sub { my $C=shift;
                            do { my @gather;
                                    eval { push @gather, Cursor::lazymap(sub { my $C=$_[0];
                                        Cursor::lazymap(sub { my $C=$_[0];
                                            Cursor::lazymap(sub { my $C=$_[0];
                                                Cursor::lazymap(sub { my $C=$_[0];
                                                    $C->_REDUCE('cintlistbody sm0p_49')
                                                }, $C->_SUBSUME(['ws'], sub {
                                                    my $C = shift()->cursor_fresh;
                                                    $C->ws
                                                }))
                                            }, $C->_SUBSUME(['cintlistbody'], sub {
                                                my $C = shift()->cursor_fresh;
                                                $C->cintlistbody
                                            }))
                                        }, $C->_SUBSUME(['ws'], sub {
                                            my $C = shift()->cursor_fresh;
                                            $C->ws
                                        }))
                                    }, $C->_EXACT(','))} 
                                    or
                                    eval { push @gather, Cursor::lazymap(sub { my $C=$_[0];
                                        $C->_REDUCE('cintlistbody sm0p_50')
                                    }, $C->_EXACT(''))} or do { die $@ if $@ };
                              @gather;
                            }
                    })
                }, $C->_SUBSUME(['ws'], sub {
                    my $C = shift()->cursor_fresh;
                    $C->ws
                }))
            }, $C->_SUBSUME(['cint'], sub {
                my $C = shift()->cursor_fresh;
                $C->cint
            }))
        }, $C->_SUBSUME(['ws'], sub {
            my $C = shift()->cursor_fresh;
            $C->ws
        }))
    );
}

## token nativeint
##      token nativeint {
##          <digits> {*}  #= sm0p_51
##      
##      }

sub nativeint {
    my $self = shift;
    local $CTX = $self->callm() if $::DEBUG & DEBUG::trace_call;
    if ($self->{_peek}) {
        return $self->_AUTOLEXpeek('nativeint',$retree)
    }


    my $C = $self;

    $self->_MATCHIFY(
        Cursor::lazymap(sub { my $C=$_[0];
            $C->_REDUCE('nativeint sm0p_51')
        }, $C->_SUBSUME(['digits'], sub {
            my $C = shift()->cursor_fresh;
            $C->digits
        }))
    );
}

## token nativeint_list
##      token nativeint_list {
##          '(' <nativeint_list_body>
##          ')' {*}  #= sm0p_52
##      
##        ||'' {*}  #= sm0p_53
##      
##      }

sub nativeint_list {
    my $self = shift;
    local $CTX = $self->callm() if $::DEBUG & DEBUG::trace_call;
    if ($self->{_peek}) {
        return $self->_AUTOLEXpeek('nativeint_list',$retree)
    }


    my $C = $self;

    $self->_MATCHIFY(
        do { my @gather;
                eval { push @gather, Cursor::lazymap(sub { my $C=$_[0];
                    Cursor::lazymap(sub { my $C=$_[0];
                        Cursor::lazymap(sub { my $C=$_[0];
                            $C->_REDUCE('nativeint_list sm0p_52')
                        }, $C->_EXACT(')'))
                    }, $C->_SUBSUME(['nativeint_list_body'], sub {
                        my $C = shift()->cursor_fresh;
                        $C->nativeint_list_body
                    }))
                }, $C->_EXACT('('))} 
                or
                eval { push @gather, Cursor::lazymap(sub { my $C=$_[0];
                    $C->_REDUCE('nativeint_list sm0p_53')
                }, $C->_EXACT(''))} or do { die $@ if $@ };
          @gather;
        }
    );
}

## token nativeint_list_body
##      token nativeint_list_body {
##          <ws> <nativeint> <ws>
##          [
##           ',' <ws> <nativeint_list_body> <ws>
##           {*}  #= sm0p_54
##      
##         ||'' {*}  #= sm0p_55
##      
##          ]
##      }

sub nativeint_list_body {
    my $self = shift;
    local $CTX = $self->callm() if $::DEBUG & DEBUG::trace_call;
    if ($self->{_peek}) {
        return $self->_AUTOLEXpeek('nativeint_list_body',$retree)
    }


    my $C = $self;
    $C->{'ws'} = [];

    $self->_MATCHIFY(
        Cursor::lazymap(sub { my $C=$_[0];
            Cursor::lazymap(sub { my $C=$_[0];
                Cursor::lazymap(sub { my $C=$_[0];
                    $C->_BRACKET( sub { my $C=shift;
                            do { my @gather;
                                    eval { push @gather, Cursor::lazymap(sub { my $C=$_[0];
                                        Cursor::lazymap(sub { my $C=$_[0];
                                            Cursor::lazymap(sub { my $C=$_[0];
                                                Cursor::lazymap(sub { my $C=$_[0];
                                                    $C->_REDUCE('nativeint_list_body sm0p_54')
                                                }, $C->_SUBSUME(['ws'], sub {
                                                    my $C = shift()->cursor_fresh;
                                                    $C->ws
                                                }))
                                            }, $C->_SUBSUME(['nativeint_list_body'], sub {
                                                my $C = shift()->cursor_fresh;
                                                $C->nativeint_list_body
                                            }))
                                        }, $C->_SUBSUME(['ws'], sub {
                                            my $C = shift()->cursor_fresh;
                                            $C->ws
                                        }))
                                    }, $C->_EXACT(','))} 
                                    or
                                    eval { push @gather, Cursor::lazymap(sub { my $C=$_[0];
                                        $C->_REDUCE('nativeint_list_body sm0p_55')
                                    }, $C->_EXACT(''))} or do { die $@ if $@ };
                              @gather;
                            }
                    })
                }, $C->_SUBSUME(['ws'], sub {
                    my $C = shift()->cursor_fresh;
                    $C->ws
                }))
            }, $C->_SUBSUME(['nativeint'], sub {
                my $C = shift()->cursor_fresh;
                $C->nativeint
            }))
        }, $C->_SUBSUME(['ws'], sub {
            my $C = shift()->cursor_fresh;
            $C->ws
        }))
    );
}

## token name
##      token name {<[a-zA-Z_]>\w*}

sub name {
    my $self = shift;
    local $CTX = $self->callm() if $::DEBUG & DEBUG::trace_call;
    if ($self->{_peek}) {
        return $self->_AUTOLEXpeek('name',$retree)
    }


    my $C = $self;

    $self->_MATCHIFY(
        Cursor::lazymap(sub { my $C=$_[0];
            $C->_STARr(sub { my $C=shift;
                $C->_ALNUM()
            })
        }, $C->_CCLASS(qr/^[a-zA-Z_]$/))
    );
}
## token digits
##      token digits {\d+}

sub digits {
    my $self = shift;
    local $CTX = $self->callm() if $::DEBUG & DEBUG::trace_call;
    if ($self->{_peek}) {
        return $self->_AUTOLEXpeek('digits',$retree)
    }


    my $C = $self;

    $self->_MATCHIFY(
        $C->_PLUSr(sub { my $C=shift;
            $C->_DIGIT()
        })
    );
}
BEGIN {
    $retree = YAML::XS::Load(<<'RETREE_END');
---
TOP: !!perl/hash:RE
  decl: []
  kind: token
  min: 74077
  re: !!perl/hash:RE_sequence
    a: 0
    i: 0
    min: 74077
    zyg:
    - !!perl/hash:RE_method
      a: 0
      i: 0
      min: 12345
      name: ws
      rest: ''
    - !!perl/hash:RE_method
      a: 0
      i: 0
      min: 12345
      name: name
      rest: ''
    - !!perl/hash:RE_method
      a: 0
      i: 0
      min: 12345
      name: ws
      rest: ''
    - !!perl/hash:RE_string
      a: 0
      i: 0
      min: 1
      text: =
    - !!perl/hash:RE_method
      a: 0
      i: 0
      min: 12345
      name: ws
      rest: ''
    - !!perl/hash:RE_string
      a: 0
      i: 0
      min: 6
      text: q:sm0p
    - !!perl/hash:RE_method
      a: 0
      i: 0
      min: 12345
      name: frame
      rest: ''
    - !!perl/hash:RE_method
      a: 0
      i: 0
      min: 12345
      name: ws
      rest: ''
    - !!perl/hash:RE_quantified_atom
      atom: !!perl/hash:RE_string
        a: 0
        i: 0
        min: 1
        text: ;
      min: 0
      quant:
      - '?'
      - ':'
      - ''
      - 0
    - !!perl/hash:RE_method_internal
      args: '''TOP sm0p_0'''
      max: 0
      min: 0
      name: _REDUCE
capturize: !!perl/hash:RE
  decl: []
  kind: token
  min: 148171
  re: !!perl/hash:RE_sequence
    a: 0
    i: 0
    min: 148171
    zyg:
    - !!perl/hash:RE_method
      a: 0
      i: 0
      min: 12345
      name: ws
      rest: ''
    - !!perl/hash:RE_string
      a: 0
      i: 0
      min: 22
      text: SMOP__SLIME__Capturize
    - !!perl/hash:RE_string
      a: 0
      i: 0
      min: 1
      text: .
    - !!perl/hash:RE_string
      a: 0
      i: 0
      min: 3
      text: new
    - !!perl/hash:RE_string
      a: 0
      i: 0
      min: 1
      text: (
    - !!perl/hash:RE_method
      a: 0
      i: 0
      min: 12345
      name: ws
      rest: ''
    - !!perl/hash:RE_method
      a: 0
      i: 0
      min: 12345
      name: cint1
      rest: ''
    - !!perl/hash:RE_method
      a: 0
      i: 0
      min: 12345
      name: ws
      rest: ''
    - !!perl/hash:RE_string
      a: 0
      i: 0
      min: 1
      text: ','
    - !!perl/hash:RE_method
      a: 0
      i: 0
      min: 12345
      name: ws
      rest: ''
    - !!perl/hash:RE_method
      a: 0
      i: 0
      min: 12345
      name: cintlist1
      rest: ''
    - !!perl/hash:RE_method
      a: 0
      i: 0
      min: 12345
      name: ws
      rest: ''
    - !!perl/hash:RE_string
      a: 0
      i: 0
      min: 1
      text: ','
    - !!perl/hash:RE_method
      a: 0
      i: 0
      min: 12345
      name: ws
      rest: ''
    - !!perl/hash:RE_method
      a: 0
      i: 0
      min: 12345
      name: cintlist2
      rest: ''
    - !!perl/hash:RE_method
      a: 0
      i: 0
      min: 12345
      name: ws
      rest: ''
    - !!perl/hash:RE_string
      a: 0
      i: 0
      min: 1
      text: ','
    - !!perl/hash:RE_method
      a: 0
      i: 0
      min: 12345
      name: ws
      rest: ''
    - !!perl/hash:RE_method
      a: 0
      i: 0
      min: 12345
      name: cint2
      rest: ''
    - !!perl/hash:RE_string
      a: 0
      i: 0
      min: 1
      text: )
    - !!perl/hash:RE_method_internal
      args: '''capturize sm0p_39'''
      max: 0
      min: 0
      name: _REDUCE
cint: !!perl/hash:RE
  decl: []
  kind: token
  min: 12345
  re: !!perl/hash:RE_first
    a: 0
    i: 0
    min: 12345
    zyg:
    - !!perl/hash:RE_sequence
      a: 0
      i: 0
      min: 12345
      zyg:
      - !!perl/hash:RE_method
        a: 0
        i: 0
        min: 12345
        name: digits
        rest: ''
      - !!perl/hash:RE_method_internal
        args: '''cint sm0p_42'''
        max: 0
        min: 0
        name: _REDUCE
    - !!perl/hash:RE_sequence
      a: 0
      i: 0
      min: 12346
      zyg:
      - !!perl/hash:RE_double
        a: 0
        i: 0
        min: 1
        text: '`'
      - !!perl/hash:RE_method
        a: 0
        i: 0
        min: 12345
        name: name
        rest: ''
      - !!perl/hash:RE_method_internal
        args: '''cint sm0p_43'''
        max: 0
        min: 0
        name: _REDUCE
cint1: !!perl/hash:RE
  decl: []
  kind: token
  min: 12345
  re: !!perl/hash:RE_sequence
    a: 0
    i: 0
    min: 12345
    zyg:
    - !!perl/hash:RE_method
      a: 0
      i: 0
      min: 12345
      name: cint
      rest: ''
    - !!perl/hash:RE_method_internal
      args: '''cint1 sm0p_40'''
      max: 0
      min: 0
      name: _REDUCE
cint2: !!perl/hash:RE
  decl: []
  kind: token
  min: 12345
  re: !!perl/hash:RE_sequence
    a: 0
    i: 0
    min: 12345
    zyg:
    - !!perl/hash:RE_method
      a: 0
      i: 0
      min: 12345
      name: cint
      rest: ''
    - !!perl/hash:RE_method_internal
      args: '''cint2 sm0p_41'''
      max: 0
      min: 0
      name: _REDUCE
cintlist: !!perl/hash:RE
  decl: []
  kind: token
  min: 0
  re: !!perl/hash:RE_first
    a: 0
    i: 0
    min: 0
    zyg:
    - !!perl/hash:RE_sequence
      a: 0
      i: 0
      min: 2
      zyg:
      - !!perl/hash:RE_string
        a: 0
        i: 0
        min: 1
        text: (
      - !!perl/hash:RE_bracket
        decl: []
        min: 1
        re: !!perl/hash:RE_first
          a: 0
          i: 0
          min: 1
          zyg:
          - !!perl/hash:RE_sequence
            a: 0
            i: 0
            min: 12346
            zyg:
            - !!perl/hash:RE_method
              a: 0
              i: 0
              min: 12345
              name: cintlistbody
              rest: ''
            - !!perl/hash:RE_string
              a: 0
              i: 0
              min: 1
              text: )
            - !!perl/hash:RE_method_internal
              args: '''cintlist sm0p_46'''
              max: 0
              min: 0
              name: _REDUCE
          - !!perl/hash:RE_sequence
            a: 0
            i: 0
            min: 1
            zyg:
            - !!perl/hash:RE_string
              a: 0
              i: 0
              min: 1
              text: )
            - !!perl/hash:RE_method_internal
              args: '''cintlist sm0p_47'''
              max: 0
              min: 0
              name: _REDUCE
    - !!perl/hash:RE_sequence
      a: 0
      i: 0
      min: 0
      zyg:
      - !!perl/hash:RE_string
        a: 0
        i: 0
        min: 0
        text: ''
      - !!perl/hash:RE_method_internal
        args: '''cintlist sm0p_48'''
        max: 0
        min: 0
        name: _REDUCE
cintlist1: !!perl/hash:RE
  decl: []
  kind: token
  min: 12345
  re: !!perl/hash:RE_sequence
    a: 0
    i: 0
    min: 12345
    zyg:
    - !!perl/hash:RE_method
      a: 0
      i: 0
      min: 12345
      name: cintlist
      rest: ''
    - !!perl/hash:RE_method_internal
      args: '''cintlist1 sm0p_44'''
      max: 0
      min: 0
      name: _REDUCE
cintlist2: !!perl/hash:RE
  decl: []
  kind: token
  min: 12345
  re: !!perl/hash:RE_sequence
    a: 0
    i: 0
    min: 12345
    zyg:
    - !!perl/hash:RE_method
      a: 0
      i: 0
      min: 12345
      name: cintlist
      rest: ''
    - !!perl/hash:RE_method_internal
      args: '''cintlist2 sm0p_45'''
      max: 0
      min: 0
      name: _REDUCE
cintlistbody: !!perl/hash:RE
  decl: []
  kind: token
  min: 37035
  re: !!perl/hash:RE_sequence
    a: 0
    i: 0
    min: 37035
    zyg:
    - !!perl/hash:RE_method
      a: 0
      i: 0
      min: 12345
      name: ws
      rest: ''
    - !!perl/hash:RE_method
      a: 0
      i: 0
      min: 12345
      name: cint
      rest: ''
    - !!perl/hash:RE_method
      a: 0
      i: 0
      min: 12345
      name: ws
      rest: ''
    - !!perl/hash:RE_bracket
      decl: []
      min: 0
      re: !!perl/hash:RE_first
        a: 0
        i: 0
        min: 0
        zyg:
        - !!perl/hash:RE_sequence
          a: 0
          i: 0
          min: 37036
          zyg:
          - !!perl/hash:RE_string
            a: 0
            i: 0
            min: 1
            text: ','
          - !!perl/hash:RE_method
            a: 0
            i: 0
            min: 12345
            name: ws
            rest: ''
          - !!perl/hash:RE_method
            a: 0
            i: 0
            min: 12345
            name: cintlistbody
            rest: ''
          - !!perl/hash:RE_method
            a: 0
            i: 0
            min: 12345
            name: ws
            rest: ''
          - !!perl/hash:RE_method_internal
            args: '''cintlistbody sm0p_49'''
            max: 0
            min: 0
            name: _REDUCE
        - !!perl/hash:RE_sequence
          a: 0
          i: 0
          min: 0
          zyg:
          - !!perl/hash:RE_string
            a: 0
            i: 0
            min: 0
            text: ''
          - !!perl/hash:RE_method_internal
            args: '''cintlistbody sm0p_50'''
            max: 0
            min: 0
            name: _REDUCE
digits: !!perl/hash:RE
  decl: []
  kind: token
  min: 1
  re: !!perl/hash:RE_quantified_atom
    atom: !!perl/hash:RE_meta
      a: 0
      i: 0
      min: 1
      text: \d
    min: 1
    quant:
    - +
    - ':'
    - ''
    - 1
frame: !!perl/hash:RE
  decl: []
  kind: token
  min: 61727
  re: !!perl/hash:RE_sequence
    a: 0
    i: 0
    min: 61727
    zyg:
    - !!perl/hash:RE_method
      a: 0
      i: 0
      min: 12345
      name: ws
      rest: ''
    - !!perl/hash:RE_string
      a: 0
      i: 0
      min: 1
      text: '{'
    - !!perl/hash:RE_method
      a: 0
      i: 0
      min: 12345
      name: ws
      rest: ''
    - !!perl/hash:RE_method
      a: 0
      i: 0
      min: 12345
      name: nodes
      rest: ''
    - !!perl/hash:RE_method
      a: 0
      i: 0
      min: 12345
      name: ws
      rest: ''
    - !!perl/hash:RE_string
      a: 0
      i: 0
      min: 1
      text: '}'
    - !!perl/hash:RE_method
      a: 0
      i: 0
      min: 12345
      name: ws
      rest: ''
    - !!perl/hash:RE_method_internal
      args: '''frame sm0p_1'''
      max: 0
      min: 0
      name: _REDUCE
idconst: !!perl/hash:RE
  decl: []
  kind: token
  min: 12345
  re: !!perl/hash:RE_sequence
    a: 0
    i: 0
    min: 12345
    zyg:
    - !!perl/hash:RE_method
      a: 0
      i: 0
      min: 12345
      name: idconst_list
      rest: ''
    - !!perl/hash:RE_method_internal
      args: '''idconst sm0p_38'''
      max: 0
      min: 0
      name: _REDUCE
idconst_list: !!perl/hash:RE
  decl: []
  kind: token
  min: 3
  re: !!perl/hash:RE_bindpos
    atom: !!perl/hash:RE_paren
      decl: []
      min: 3
      re: &1 !!perl/hash:RE_any
        altname: idconst_list_01
        min: 3
        name: idconst_list_01
        zyg:
        - !!perl/hash:RE_string
          a: 0
          alt: idconst_list_01 0
          i: 0
          min: 3
          text: new
        - !!perl/hash:RE_string
          a: 0
          alt: idconst_list_01 1
          i: 0
          min: 7
          text: lexical
        - !!perl/hash:RE_string
          a: 0
          alt: idconst_list_01 2
          i: 0
          min: 4
          text: back
        - !!perl/hash:RE_string
          a: 0
          alt: idconst_list_01 3
          i: 0
          min: 7
          text: capture
        - !!perl/hash:RE_string
          a: 0
          alt: idconst_list_01 4
          i: 0
          min: 12
          text: continuation
        - !!perl/hash:RE_string
          a: 0
          alt: idconst_list_01 5
          i: 0
          min: 9
          text: continues
        - !!perl/hash:RE_string
          a: 0
          alt: idconst_list_01 6
          i: 0
          min: 4
          text: copy
        - !!perl/hash:RE_string
          a: 0
          alt: idconst_list_01 7
          i: 0
          min: 7
          text: current
        - !!perl/hash:RE_string
          a: 0
          alt: idconst_list_01 8
          i: 0
          min: 5
          text: debug
        - !!perl/hash:RE_string
          a: 0
          alt: idconst_list_01 9
          i: 0
          min: 4
          text: drop
        - !!perl/hash:RE_string
          a: 0
          alt: idconst_list_01 10
          i: 0
          min: 10
          text: DESTROYALL
        - !!perl/hash:RE_string
          a: 0
          alt: idconst_list_01 11
          i: 0
          min: 5
          text: FETCH
        - !!perl/hash:RE_string
          a: 0
          alt: idconst_list_01 12
          i: 0
          min: 5
          text: STORE
        - !!perl/hash:RE_string
          a: 0
          alt: idconst_list_01 13
          i: 0
          min: 4
          text: eval
        - !!perl/hash:RE_string
          a: 0
          alt: idconst_list_01 14
          i: 0
          min: 6
          text: forget
        - !!perl/hash:RE_string
          a: 0
          alt: idconst_list_01 15
          i: 0
          min: 4
          text: free
        - !!perl/hash:RE_string
          a: 0
          alt: idconst_list_01 16
          i: 0
          min: 4
          text: goto
        - !!perl/hash:RE_string
          a: 0
          alt: idconst_list_01 17
          i: 0
          min: 8
          text: has_next
        - !!perl/hash:RE_string
          a: 0
          alt: idconst_list_01 18
          i: 0
          min: 10
          text: identifier
        - !!perl/hash:RE_string
          a: 0
          alt: idconst_list_01 19
          i: 0
          min: 4
          text: jail
        - !!perl/hash:RE_string
          a: 0
          alt: idconst_list_01 20
          i: 0
          min: 7
          text: lexical
        - !!perl/hash:RE_string
          a: 0
          alt: idconst_list_01 21
          i: 0
          min: 4
          text: loop
        - !!perl/hash:RE_string
          a: 0
          alt: idconst_list_01 22
          i: 0
          min: 14
          text: move_capturize
        - !!perl/hash:RE_string
          a: 0
          alt: idconst_list_01 23
          i: 0
          min: 15
          text: move_identifier
        - !!perl/hash:RE_string
          a: 0
          alt: idconst_list_01 24
          i: 0
          min: 14
          text: move_responder
        - !!perl/hash:RE_string
          a: 0
          alt: idconst_list_01 25
          i: 0
          min: 3
          text: new
        - !!perl/hash:RE_string
          a: 0
          alt: idconst_list_01 26
          i: 0
          min: 4
          text: next
        - !!perl/hash:RE_string
          a: 0
          alt: idconst_list_01 27
          i: 0
          min: 4
          text: past
        - !!perl/hash:RE_string
          a: 0
          alt: idconst_list_01 28
          i: 0
          min: 4
          text: push
        - !!perl/hash:RE_string
          a: 0
          alt: idconst_list_01 29
          i: 0
          min: 9
          text: responder
        - !!perl/hash:RE_string
          a: 0
          alt: idconst_list_01 30
          i: 0
          min: 6
          text: result
        - !!perl/hash:RE_string
          a: 0
          alt: idconst_list_01 31
          i: 0
          min: 4
          text: setr
        - !!perl/hash:RE_string
          a: 0
          alt: idconst_list_01 32
          i: 0
          min: 5
          text: outer
    min: 3
    var: 0
idconst_list_01: *1
identifier: !!perl/hash:RE
  decl: []
  kind: token
  min: 14
  re: !!perl/hash:RE_first
    a: 0
    i: 0
    min: 14
    zyg:
    - !!perl/hash:RE_sequence
      a: 0
      i: 0
      min: 12346
      zyg:
      - !!perl/hash:RE_string
        a: 0
        i: 0
        min: 1
        text: $
      - !!perl/hash:RE_method
        a: 0
        i: 0
        min: 12345
        name: name
        rest: ''
      - !!perl/hash:RE_method_internal
        args: '''identifier sm0p_34'''
        max: 0
        min: 0
        name: _REDUCE
    - !!perl/hash:RE_sequence
      a: 0
      i: 0
      min: 14
      zyg:
      - !!perl/hash:RE_string
        a: 0
        i: 0
        min: 13
        text: q:identifier[
      - !!perl/hash:RE_bindpos
        atom: !!perl/hash:RE_paren
          decl: []
          min: 0
          re: !!perl/hash:RE_quantified_atom
            atom: !!perl/hash:RE_method
              a: 0
              i: 0
              min: 12345
              name: notbracket_or_backslashed
              nobind: 1
              rest: ''
            min: 0
            quant:
            - '*'
            - ':'
            - ''
            - 0
        min: 0
        var: 0
      - !!perl/hash:RE_string
        a: 0
        i: 0
        min: 1
        text: ']'
      - !!perl/hash:RE_method_internal
        args: '''identifier sm0p_35'''
        max: 0
        min: 0
        name: _REDUCE
    - !!perl/hash:RE_sequence
      a: 0
      i: 0
      min: 12345
      zyg:
      - !!perl/hash:RE_method
        a: 0
        i: 0
        min: 12345
        name: idconst
        rest: ''
      - !!perl/hash:RE_method_internal
        args: '''identifier sm0p_36'''
        max: 0
        min: 0
        name: _REDUCE
    - !!perl/hash:RE_sequence
      a: 0
      i: 0
      min: 12345
      zyg:
      - !!perl/hash:RE_method
        a: 0
        i: 0
        min: 12345
        name: name
        rest: ''
      - !!perl/hash:RE_method_internal
        args: '''identifier sm0p_37'''
        max: 0
        min: 0
        name: _REDUCE
identifier2: !!perl/hash:RE
  decl: []
  kind: token
  min: 12345
  re: !!perl/hash:RE_sequence
    a: 0
    i: 0
    min: 12345
    zyg:
    - !!perl/hash:RE_method
      a: 0
      i: 0
      min: 12345
      name: identifier
      rest: ''
    - !!perl/hash:RE_method_internal
      args: '''identifier2 sm0p_33'''
      max: 0
      min: 0
      name: _REDUCE
invocant: !!perl/hash:RE
  decl: []
  kind: token
  min: 49381
  re: !!perl/hash:RE_sequence
    a: 0
    i: 0
    min: 49381
    zyg:
    - !!perl/hash:RE_method
      a: 0
      i: 0
      min: 12345
      name: ws
      rest: ''
    - !!perl/hash:RE_method
      a: 0
      i: 0
      min: 12345
      name: identifier
      rest: ''
    - !!perl/hash:RE_method
      a: 0
      i: 0
      min: 12345
      name: ws
      rest: ''
    - !!perl/hash:RE_string
      a: 0
      i: 0
      min: 1
      text: ':'
    - !!perl/hash:RE_method
      a: 0
      i: 0
      min: 12345
      name: ws
      rest: ''
    - !!perl/hash:RE_method_internal
      args: '''invocant sm0p_15'''
      max: 0
      min: 0
      name: _REDUCE
label: !!perl/hash:RE
  decl: []
  kind: token
  min: 37036
  re: !!perl/hash:RE_sequence
    a: 0
    i: 0
    min: 37036
    zyg:
    - !!perl/hash:RE_method
      a: 0
      i: 0
      min: 12345
      name: ws
      rest: ''
    - !!perl/hash:RE_method
      a: 0
      i: 0
      min: 12345
      name: name
      rest: ''
    - !!perl/hash:RE_string
      a: 0
      i: 0
      min: 1
      text: ':'
    - !!perl/hash:RE_method
      a: 0
      i: 0
      min: 12345
      name: ws
      rest: ''
    - !!perl/hash:RE_method_internal
      args: '''label sm0p_5'''
      max: 0
      min: 0
      name: _REDUCE
name: !!perl/hash:RE
  decl: []
  kind: token
  min: 1
  re: !!perl/hash:RE_sequence
    a: 0
    i: 0
    min: 1
    zyg:
    - !!perl/hash:RE_cclass
      a: 0
      i: 0
      min: 1
      text: '[a-zA-Z_]'
    - !!perl/hash:RE_quantified_atom
      atom: !!perl/hash:RE_meta
        a: 0
        i: 0
        min: 1
        text: \w
      min: 0
      quant:
      - '*'
      - ':'
      - ''
      - 0
named: !!perl/hash:RE
  decl: []
  kind: token
  min: 0
  re: !!perl/hash:RE_first
    a: 0
    i: 0
    min: 0
    zyg:
    - !!perl/hash:RE_sequence
      a: 0
      i: 0
      min: 12345
      zyg:
      - !!perl/hash:RE_method
        a: 0
        i: 0
        min: 12345
        name: pairs
        rest: ''
      - !!perl/hash:RE_method_internal
        args: '''named sm0p_28'''
        max: 0
        min: 0
        name: _REDUCE
    - !!perl/hash:RE_method_internal
      args: '''named sm0p_29'''
      max: 0
      min: 0
      name: _REDUCE
nativeint: !!perl/hash:RE
  decl: []
  kind: token
  min: 12345
  re: !!perl/hash:RE_sequence
    a: 0
    i: 0
    min: 12345
    zyg:
    - !!perl/hash:RE_method
      a: 0
      i: 0
      min: 12345
      name: digits
      rest: ''
    - !!perl/hash:RE_method_internal
      args: '''nativeint sm0p_51'''
      max: 0
      min: 0
      name: _REDUCE
nativeint_list: !!perl/hash:RE
  decl: []
  kind: token
  min: 0
  re: !!perl/hash:RE_first
    a: 0
    i: 0
    min: 0
    zyg:
    - !!perl/hash:RE_sequence
      a: 0
      i: 0
      min: 12347
      zyg:
      - !!perl/hash:RE_string
        a: 0
        i: 0
        min: 1
        text: (
      - !!perl/hash:RE_method
        a: 0
        i: 0
        min: 12345
        name: nativeint_list_body
        rest: ''
      - !!perl/hash:RE_string
        a: 0
        i: 0
        min: 1
        text: )
      - !!perl/hash:RE_method_internal
        args: '''nativeint_list sm0p_52'''
        max: 0
        min: 0
        name: _REDUCE
    - !!perl/hash:RE_sequence
      a: 0
      i: 0
      min: 0
      zyg:
      - !!perl/hash:RE_string
        a: 0
        i: 0
        min: 0
        text: ''
      - !!perl/hash:RE_method_internal
        args: '''nativeint_list sm0p_53'''
        max: 0
        min: 0
        name: _REDUCE
nativeint_list_body: !!perl/hash:RE
  decl: []
  kind: token
  min: 37035
  re: !!perl/hash:RE_sequence
    a: 0
    i: 0
    min: 37035
    zyg:
    - !!perl/hash:RE_method
      a: 0
      i: 0
      min: 12345
      name: ws
      rest: ''
    - !!perl/hash:RE_method
      a: 0
      i: 0
      min: 12345
      name: nativeint
      rest: ''
    - !!perl/hash:RE_method
      a: 0
      i: 0
      min: 12345
      name: ws
      rest: ''
    - !!perl/hash:RE_bracket
      decl: []
      min: 0
      re: !!perl/hash:RE_first
        a: 0
        i: 0
        min: 0
        zyg:
        - !!perl/hash:RE_sequence
          a: 0
          i: 0
          min: 37036
          zyg:
          - !!perl/hash:RE_string
            a: 0
            i: 0
            min: 1
            text: ','
          - !!perl/hash:RE_method
            a: 0
            i: 0
            min: 12345
            name: ws
            rest: ''
          - !!perl/hash:RE_method
            a: 0
            i: 0
            min: 12345
            name: nativeint_list_body
            rest: ''
          - !!perl/hash:RE_method
            a: 0
            i: 0
            min: 12345
            name: ws
            rest: ''
          - !!perl/hash:RE_method_internal
            args: '''nativeint_list_body sm0p_54'''
            max: 0
            min: 0
            name: _REDUCE
        - !!perl/hash:RE_sequence
          a: 0
          i: 0
          min: 0
          zyg:
          - !!perl/hash:RE_string
            a: 0
            i: 0
            min: 0
            text: ''
          - !!perl/hash:RE_method_internal
            args: '''nativeint_list_body sm0p_55'''
            max: 0
            min: 0
            name: _REDUCE
nativestring: !!perl/hash:RE
  decl: []
  kind: token
  min: 2
  re: !!perl/hash:RE_sequence
    a: 0
    i: 0
    min: 2
    zyg:
    - !!perl/hash:RE_string
      a: 0
      i: 0
      min: 1
      text: '"'
    - !!perl/hash:RE_bindpos
      atom: !!perl/hash:RE_paren
        decl: []
        min: 0
        re: !!perl/hash:RE_quantified_atom
          atom: !!perl/hash:RE_method
            a: 0
            i: 0
            min: 12345
            name: notquote_or_backslashed
            nobind: 1
            rest: ''
          min: 0
          quant:
          - '*'
          - ':'
          - ''
          - 0
      min: 0
      var: 0
    - !!perl/hash:RE_string
      a: 0
      i: 0
      min: 1
      text: '"'
    - !!perl/hash:RE_method_internal
      args: '''nativestring sm0p_19'''
      max: 0
      min: 0
      name: _REDUCE
node: !!perl/hash:RE
  decl: []
  kind: token
  min: 12345
  re: !!perl/hash:RE_sequence
    a: 0
    i: 0
    min: 12345
    zyg:
    - !!perl/hash:RE_quantified_atom
      atom: !!perl/hash:RE_method
        a: 0
        i: 0
        min: 12345
        name: label
        rest: ''
      min: 0
      quant:
      - '?'
      - ':'
      - ''
      - 0
    - !!perl/hash:RE_bracket
      decl: []
      min: 12345
      re: !!perl/hash:RE_first
        a: 0
        i: 0
        min: 12345
        zyg:
        - !!perl/hash:RE_sequence
          a: 0
          i: 0
          min: 12345
          zyg:
          - !!perl/hash:RE_method
            a: 0
            i: 0
            min: 12345
            name: node_empty
            rest: ''
          - !!perl/hash:RE_method_internal
            args: '''node sm0p_6'''
            max: 0
            min: 0
            name: _REDUCE
        - !!perl/hash:RE_sequence
          a: 0
          i: 0
          min: 12345
          zyg:
          - !!perl/hash:RE_method
            a: 0
            i: 0
            min: 12345
            name: node_result
            rest: ''
          - !!perl/hash:RE_method_internal
            args: '''node sm0p_7'''
            max: 0
            min: 0
            name: _REDUCE
        - !!perl/hash:RE_sequence
          a: 0
          i: 0
          min: 12345
          zyg:
          - !!perl/hash:RE_method
            a: 0
            i: 0
            min: 12345
            name: node_capturized
            rest: ''
          - !!perl/hash:RE_method_internal
            args: '''node sm0p_8'''
            max: 0
            min: 0
            name: _REDUCE
        - !!perl/hash:RE_sequence
          a: 0
          i: 0
          min: 12345
          zyg:
          - !!perl/hash:RE_method
            a: 0
            i: 0
            min: 12345
            name: node_full
            rest: ''
          - !!perl/hash:RE_method_internal
            args: '''node sm0p_9'''
            max: 0
            min: 0
            name: _REDUCE
    - !!perl/hash:RE_method_internal
      args: '''node sm0p_10'''
      max: 0
      min: 0
      name: _REDUCE
node_capturized: !!perl/hash:RE
  decl: []
  kind: token
  min: 98765
  re: !!perl/hash:RE_sequence
    a: 0
    i: 0
    min: 98765
    zyg:
    - !!perl/hash:RE_method
      a: 0
      i: 0
      min: 12345
      name: ws
      rest: ''
    - !!perl/hash:RE_method
      a: 0
      i: 0
      min: 12345
      name: responder
      rest: ''
    - !!perl/hash:RE_string
      a: 0
      i: 0
      min: 1
      text: .
    - !!perl/hash:RE_method
      a: 0
      i: 0
      min: 12345
      name: identifier
      rest: ''
    - !!perl/hash:RE_string
      a: 0
      i: 0
      min: 1
      text: (
    - !!perl/hash:RE_method
      a: 0
      i: 0
      min: 12345
      name: ws
      rest: ''
    - !!perl/hash:RE_string
      a: 0
      i: 0
      min: 1
      text: '|'
    - !!perl/hash:RE_method
      a: 0
      i: 0
      min: 12345
      name: identifier2
      rest: ''
    - !!perl/hash:RE_method
      a: 0
      i: 0
      min: 12345
      name: ws
      rest: ''
    - !!perl/hash:RE_string
      a: 0
      i: 0
      min: 1
      text: )
    - !!perl/hash:RE_method
      a: 0
      i: 0
      min: 12345
      name: ws
      rest: ''
    - !!perl/hash:RE_string
      a: 0
      i: 0
      min: 1
      text: ;
    - !!perl/hash:RE_method
      a: 0
      i: 0
      min: 12345
      name: ws
      rest: ''
    - !!perl/hash:RE_method_internal
      args: '''node_capturized sm0p_13'''
      max: 0
      min: 0
      name: _REDUCE
node_empty: !!perl/hash:RE
  decl: []
  kind: token
  min: 12346
  re: !!perl/hash:RE_sequence
    a: 0
    i: 0
    min: 12346
    zyg:
    - !!perl/hash:RE_method
      a: 0
      i: 0
      min: 12345
      name: ws
      rest: ''
    - !!perl/hash:RE_string
      a: 0
      i: 0
      min: 1
      text: ;
    - !!perl/hash:RE_method_internal
      args: '''node_empty sm0p_11'''
      max: 0
      min: 0
      name: _REDUCE
node_full: !!perl/hash:RE
  decl: []
  kind: token
  min: 123454
  re: !!perl/hash:RE_sequence
    a: 0
    i: 0
    min: 123454
    zyg:
    - !!perl/hash:RE_method
      a: 0
      i: 0
      min: 12345
      name: ws
      rest: ''
    - !!perl/hash:RE_method
      a: 0
      i: 0
      min: 12345
      name: responder
      rest: ''
    - !!perl/hash:RE_string
      a: 0
      i: 0
      min: 1
      text: .
    - !!perl/hash:RE_method
      a: 0
      i: 0
      min: 12345
      name: identifier
      rest: ''
    - !!perl/hash:RE_string
      a: 0
      i: 0
      min: 1
      text: (
    - !!perl/hash:RE_bracket
      decl: []
      min: 0
      re: !!perl/hash:RE_first
        a: 0
        i: 0
        min: 0
        zyg:
        - !!perl/hash:RE_method
          a: 0
          i: 0
          min: 12345
          name: invocant
          rest: ''
        - !!perl/hash:RE_string
          a: 0
          i: 0
          min: 0
          text: ''
    - !!perl/hash:RE_method
      a: 0
      i: 0
      min: 12345
      name: ws
      rest: ''
    - !!perl/hash:RE_method
      a: 0
      i: 0
      min: 12345
      name: named
      rest: ''
    - !!perl/hash:RE_method
      a: 0
      i: 0
      min: 12345
      name: ws
      rest: ''
    - !!perl/hash:RE_method
      a: 0
      i: 0
      min: 12345
      name: positional
      rest: ''
    - !!perl/hash:RE_method
      a: 0
      i: 0
      min: 12345
      name: ws
      rest: ''
    - !!perl/hash:RE_string
      a: 0
      i: 0
      min: 1
      text: )
    - !!perl/hash:RE_method
      a: 0
      i: 0
      min: 12345
      name: ws
      rest: ''
    - !!perl/hash:RE_string
      a: 0
      i: 0
      min: 1
      text: ;
    - !!perl/hash:RE_method
      a: 0
      i: 0
      min: 12345
      name: ws
      rest: ''
    - !!perl/hash:RE_method_internal
      args: '''node_full sm0p_12'''
      max: 0
      min: 0
      name: _REDUCE
node_result: !!perl/hash:RE
  decl: []
  kind: token
  min: 37036
  re: !!perl/hash:RE_sequence
    a: 0
    i: 0
    min: 37036
    zyg:
    - !!perl/hash:RE_method
      a: 0
      i: 0
      min: 12345
      name: ws
      rest: ''
    - !!perl/hash:RE_method
      a: 0
      i: 0
      min: 12345
      name: value
      rest: ''
    - !!perl/hash:RE_method
      a: 0
      i: 0
      min: 12345
      name: ws
      rest: ''
    - !!perl/hash:RE_string
      a: 0
      i: 0
      min: 1
      text: ;
    - !!perl/hash:RE_method_internal
      args: '''node_result sm0p_14'''
      max: 0
      min: 0
      name: _REDUCE
nodes: !!perl/hash:RE
  decl: []
  kind: token
  min: 0
  re: !!perl/hash:RE_sequence
    a: 0
    i: 0
    min: 0
    zyg:
    - !!perl/hash:RE_method_internal
      args: '''nodes sm0p_2'''
      max: 0
      min: 0
      name: _REDUCE
    - !!perl/hash:RE_quantified_atom
      atom: !!perl/hash:RE_method
        a: 0
        i: 0
        min: 12345
        name: node
        rest: ''
      min: 0
      quant:
      - '*'
      - ':'
      - ''
      - 0
    - !!perl/hash:RE_method_internal
      args: '''nodes sm0p_3'''
      max: 0
      min: 0
      name: _REDUCE
    - !!perl/hash:RE_method_internal
      args: '''nodes sm0p_4'''
      max: 0
      min: 0
      name: _REDUCE
notbracket_or_backslashed: !!perl/hash:RE
  decl: []
  kind: token
  min: 1
  re: !!perl/hash:RE_first
    a: 0
    i: 0
    min: 1
    zyg:
    - !!perl/hash:RE_sequence
      a: 0
      i: 0
      min: 2
      zyg:
      - !!perl/hash:RE_string
        a: 0
        i: 0
        min: 1
        text: \
      - !!perl/hash:RE_meta
        a: 0
        i: 0
        min: 1
        text: .
    - !!perl/hash:RE_cclass
      a: 0
      i: 0
      min: 1
      text: -[\]]
notquote_or_backslashed: !!perl/hash:RE
  decl: []
  kind: token
  min: 1
  re: !!perl/hash:RE_first
    a: 0
    i: 0
    min: 1
    zyg:
    - !!perl/hash:RE_sequence
      a: 0
      i: 0
      min: 2
      zyg:
      - !!perl/hash:RE_string
        a: 0
        i: 0
        min: 1
        text: \
      - !!perl/hash:RE_meta
        a: 0
        i: 0
        min: 1
        text: .
    - !!perl/hash:RE_cclass
      a: 0
      i: 0
      min: 1
      text: -["]
pair: !!perl/hash:RE
  decl: []
  kind: token
  min: 49382
  re: !!perl/hash:RE_sequence
    a: 0
    i: 0
    min: 49382
    zyg:
    - !!perl/hash:RE_method
      a: 0
      i: 0
      min: 12345
      name: identifier
      rest: ''
    - !!perl/hash:RE_method
      a: 0
      i: 0
      min: 12345
      name: ws
      rest: ''
    - !!perl/hash:RE_string
      a: 0
      i: 0
      min: 2
      text: =>
    - !!perl/hash:RE_method
      a: 0
      i: 0
      min: 12345
      name: ws
      rest: ''
    - !!perl/hash:RE_method
      a: 0
      i: 0
      min: 12345
      name: identifier2
      rest: ''
    - !!perl/hash:RE_method_internal
      args: '''pair sm0p_32'''
      max: 0
      min: 0
      name: _REDUCE
pairs: !!perl/hash:RE
  decl: []
  kind: token
  min: 24690
  re: !!perl/hash:RE_sequence
    a: 0
    i: 0
    min: 24690
    zyg:
    - !!perl/hash:RE_method
      a: 0
      i: 0
      min: 12345
      name: pair
      rest: ''
    - !!perl/hash:RE_bracket
      decl: []
      min: 12345
      re: !!perl/hash:RE_first
        a: 0
        i: 0
        min: 12345
        zyg:
        - !!perl/hash:RE_sequence
          a: 0
          i: 0
          min: 37036
          zyg:
          - !!perl/hash:RE_method
            a: 0
            i: 0
            min: 12345
            name: ws
            rest: ''
          - !!perl/hash:RE_string
            a: 0
            i: 0
            min: 1
            text: ','
          - !!perl/hash:RE_method
            a: 0
            i: 0
            min: 12345
            name: ws
            rest: ''
          - !!perl/hash:RE_method
            a: 0
            i: 0
            min: 12345
            name: pairs
            rest: ''
          - !!perl/hash:RE_method_internal
            args: '''pairs sm0p_30'''
            max: 0
            min: 0
            name: _REDUCE
        - !!perl/hash:RE_sequence
          a: 0
          i: 0
          min: 12345
          zyg:
          - !!perl/hash:RE_method
            a: 0
            i: 0
            min: 12345
            name: ws
            rest: ''
          - !!perl/hash:RE_bracket
            decl: []
            min: 0
            re: !!perl/hash:RE_first
              a: 0
              i: 0
              min: 0
              zyg:
              - !!perl/hash:RE_sequence
                a: 0
                i: 0
                min: 12346
                zyg:
                - !!perl/hash:RE_string
                  a: 0
                  i: 0
                  min: 1
                  text: ','
                - !!perl/hash:RE_method
                  a: 0
                  i: 0
                  min: 12345
                  name: ws
                  rest: ''
              - !!perl/hash:RE_string
                a: 0
                i: 0
                min: 0
                text: ''
          - !!perl/hash:RE_method_internal
            args: '''pairs sm0p_31'''
            max: 0
            min: 0
            name: _REDUCE
positional: !!perl/hash:RE
  decl: []
  kind: token
  min: 0
  re: !!perl/hash:RE_first
    a: 0
    i: 0
    min: 0
    zyg:
    - !!perl/hash:RE_sequence
      a: 0
      i: 0
      min: 12345
      zyg:
      - !!perl/hash:RE_method
        a: 0
        i: 0
        min: 12345
        name: positionals
        rest: ''
      - !!perl/hash:RE_method_internal
        args: '''positional sm0p_17'''
        max: 0
        min: 0
        name: _REDUCE
    - !!perl/hash:RE_method_internal
      args: '''positional sm0p_18'''
      max: 0
      min: 0
      name: _REDUCE
positionals: !!perl/hash:RE
  decl: []
  kind: token
  min: 24690
  re: !!perl/hash:RE_sequence
    a: 0
    i: 0
    min: 24690
    zyg:
    - !!perl/hash:RE_method
      a: 0
      i: 0
      min: 12345
      name: value
      rest: ''
    - !!perl/hash:RE_bracket
      decl: []
      min: 12345
      re: !!perl/hash:RE_first
        a: 0
        i: 0
        min: 12345
        zyg:
        - !!perl/hash:RE_sequence
          a: 0
          i: 0
          min: 37036
          zyg:
          - !!perl/hash:RE_method
            a: 0
            i: 0
            min: 12345
            name: ws
            rest: ''
          - !!perl/hash:RE_string
            a: 0
            i: 0
            min: 1
            text: ','
          - !!perl/hash:RE_method
            a: 0
            i: 0
            min: 12345
            name: ws
            rest: ''
          - !!perl/hash:RE_method
            a: 0
            i: 0
            min: 12345
            name: positionals
            rest: ''
          - !!perl/hash:RE_method_internal
            args: '''positionals sm0p_26'''
            max: 0
            min: 0
            name: _REDUCE
        - !!perl/hash:RE_sequence
          a: 0
          i: 0
          min: 12345
          zyg:
          - !!perl/hash:RE_method
            a: 0
            i: 0
            min: 12345
            name: ws
            rest: ''
          - !!perl/hash:RE_method_internal
            args: '''positionals sm0p_27'''
            max: 0
            min: 0
            name: _REDUCE
responder: !!perl/hash:RE
  decl: []
  kind: token
  min: 12345
  re: !!perl/hash:RE_sequence
    a: 0
    i: 0
    min: 12345
    zyg:
    - !!perl/hash:RE_method
      a: 0
      i: 0
      min: 12345
      name: identifier
      rest: ''
    - !!perl/hash:RE_method_internal
      args: '''responder sm0p_16'''
      max: 0
      min: 0
      name: _REDUCE
value: !!perl/hash:RE
  decl: []
  kind: token
  min: 12345
  re: !!perl/hash:RE_first
    a: 0
    i: 0
    min: 12345
    zyg:
    - !!perl/hash:RE_sequence
      a: 0
      i: 0
      min: 12345
      zyg:
      - !!perl/hash:RE_method
        a: 0
        i: 0
        min: 12345
        name: frame
        rest: ''
      - !!perl/hash:RE_method_internal
        args: '''value sm0p_20'''
        max: 0
        min: 0
        name: _REDUCE
    - !!perl/hash:RE_sequence
      a: 0
      i: 0
      min: 12345
      zyg:
      - !!perl/hash:RE_method
        a: 0
        i: 0
        min: 12345
        name: nativestring
        rest: ''
      - !!perl/hash:RE_method_internal
        args: '''value sm0p_21'''
        max: 0
        min: 0
        name: _REDUCE
    - !!perl/hash:RE_sequence
      a: 0
      i: 0
      min: 12345
      zyg:
      - !!perl/hash:RE_method
        a: 0
        i: 0
        min: 12345
        name: nativeint
        rest: ''
      - !!perl/hash:RE_method_internal
        args: '''value sm0p_22'''
        max: 0
        min: 0
        name: _REDUCE
    - !!perl/hash:RE_sequence
      a: 0
      i: 0
      min: 12345
      zyg:
      - !!perl/hash:RE_method
        a: 0
        i: 0
        min: 12345
        name: capturize
        rest: ''
      - !!perl/hash:RE_method_internal
        args: '''value sm0p_23'''
        max: 0
        min: 0
        name: _REDUCE
    - !!perl/hash:RE_sequence
      a: 0
      i: 0
      min: 12345
      zyg:
      - !!perl/hash:RE_method
        a: 0
        i: 0
        min: 12345
        name: identifier
        rest: ''
      - !!perl/hash:RE_method_internal
        args: '''value sm0p_24'''
        max: 0
        min: 0
        name: _REDUCE
    - !!perl/hash:RE_sequence
      a: 0
      i: 0
      min: 12346
      zyg:
      - !!perl/hash:RE_double
        a: 0
        i: 0
        min: 1
        text: '`'
      - !!perl/hash:RE_method
        a: 0
        i: 0
        min: 12345
        name: name
        rest: ''
      - !!perl/hash:RE_method_internal
        args: '''value sm0p_25'''
        max: 0
        min: 0
        name: _REDUCE
ws: !!perl/hash:RE
  decl: []
  kind: token
  min: 0
  re: !!perl/hash:RE_quantified_atom
    atom: !!perl/hash:RE_bracket
      decl: []
      min: 1
      re: &2 !!perl/hash:RE_any
        altname: ws_01
        min: 1
        name: ws_01
        zyg:
        - !!perl/hash:RE_meta
          a: 0
          alt: ws_01 0
          i: 0
          min: 1
          text: \s
        - !!perl/hash:RE_sequence
          a: 0
          alt: ws_01 1
          i: 0
          min: 2
          zyg:
          - !!perl/hash:RE_string
            a: 0
            i: 0
            min: 1
            text: '#'
          - !!perl/hash:RE_quantified_atom
            atom: !!perl/hash:RE_meta
              a: 0
              i: 0
              min: 1
              text: \N
            min: 0
            quant:
            - '*'
            - ':'
            - ''
            - 0
          - !!perl/hash:RE_double
            a: 0
            i: 0
            min: 1
            text: \n
    min: 0
    quant:
    - '*'
    - ':'
    - ''
    - 0
ws_01: *2
RETREE_END
}
