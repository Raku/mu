
require('Perl6.MetaClass.Dispatcher');

if (Perl6 == undefined) var Perl6 = {};

Perl6.MetaClass = function (name, version, authority) {
    // meta information
    this._name      = name      || false;
    this._version   = version   || '0.0.0';
    this._authority = authority || false;
    // the guts of the metaclass
    this._MRO              = [];
    this._superclasses     = [];
    this._class_definition = { methods : {}, attributes : {} };
    this._class_data       = { methods : {}, attributes : {} };
}

Perl6.MetaClass.prototype.name = function (name) {
    if (name) this._name = name;
    return this._name;
}

Perl6.MetaClass.prototype.version = function (version) {
    if (version) this._version = version;
    return this._version;
}

Perl6.MetaClass.prototype.authority = function (authority) {
    if (authority) this._authority = authority;
    return this._authority;
}

Perl6.MetaClass.prototype.identifier = function () {
    var ident = "";
    if (this._name)      ident += this._name;
    if (this._version)   ident += "-" + this._version;    
    if (this._authority) ident += "-" + this._authority;        
    return ident;
}

Perl6.MetaClass.prototype.toString = function () {
    return "Perl6.MetaClass=[" + this.identifier() + "]";
}

// superclasses

Perl6.MetaClass.prototype.superclasses = function (superclasses) {
    if (superclasses) { 
        this._superclasses = superclasses;
        this._MRO = [];
        this.MRO();
    }
    return this._superclasses;
}

Perl6.MetaClass.prototype.is_a = function (classname) {
    if (this.name() == classname) return true;
    for (var i = 0; i < this._superclasses.length; i++) {
        if (this._superclasses[i].is_a(classname)) return true;
    }
    return false;
}

// private functions related to MRO

function _remove_empty_seqs (seqs) {
    var nonemptyseqs = [];
    for (var i = 0; i < seqs.length; i++) {
        if (seqs[i].length != 0) { 
            nonemptyseqs[nonemptyseqs.length] = seqs[i];
        }
    }   
    return nonemptyseqs; 
}

function _in_tail (seq, cand) {
    for (var i = 1; i < seq.length; i++) {
        if (seq[i] == cand) return true;
    }   
    return false;    
}

function _remove_heads_if (cand, nonemptyseqs) {
    for (var i = 0; i < nonemptyseqs.length; i++) {
        if (nonemptyseqs[i][0] == cand) {
            nonemptyseqs[i] = nonemptyseqs[i].slice(1, nonemptyseqs[i].length);
        }
    }   
    return nonemptyseqs; 
}

function merge (seqs) {
    var res = [];
    while (true) {
        // remove all empty seqences
        var nonemptyseqs = _remove_empty_seqs(seqs);
        // return the list if we have no more no-empty sequences
        if (nonemptyseqs.length == 0) {
            return res;
        }
        var cand = false;
        for (var i = 0; i < nonemptyseqs.length; i++) {
            var seq = nonemptyseqs[i];
            cand = seq[0]; 
            var nothead = false;
            for (var j = 0; j < nonemptyseqs.length; j++) {     
                var sub_seq = nonemptyseqs[j];
                if (_in_tail(sub_seq, cand)) {
                    nothead = true;
                    break;
                }
            }
            if (nothead == false) {
                // leave the loop with our canidate ...               
                break;
            }
            else {
                // otherwise, reject it ...
                cand = false;
            }
        }
        if (cand == false) {
            throw "Inconsistent hierarchy";
        }
        res[res.length] = cand;
        // now loop through our non-empties and pop 
        // off the head if it matches our canidate
        seqs = _remove_heads_if(cand, seqs);
    }    
}

Perl6.MetaClass.prototype.MRO = function () {
    if (this._MRO.length == 0) {
        var args = [[this]];
        for (var i = 0; i < this._superclasses.length; i++) {
            args[args.length] = this._superclasses[i].MRO();
        }        
        args[args.length] = this._superclasses;
        this._MRO = merge(args);
    }
    return this._MRO;
}

Perl6.MetaClass.prototype.dispatcher = function (order) {
    return new Perl6.MetaClass.Dispatcher (this, order);
}

// methods

Perl6.MetaClass.prototype.add_method = function (label, method, type) {
    if (!type || type == 'instance') {
        this._class_definition.methods[label] = method;        
    }
    else if (type == 'class') {
        this._class_data.methods[label] = method;            
    }    
    else {
        throw 'Unsupported Method Type';
    }
}

Perl6.MetaClass.prototype.get_method = function (label, type) {
    if (!type || type == 'instance') {
        return this._class_definition.methods[label];        
    }
    else if (type == 'class') {
        return this._class_data.methods[label];            
    }
    else {
        throw 'Unsupported Method Type';
    }    
}

Perl6.MetaClass.prototype.has_method = function (label, type) {
    return this.get_method(label, type) ? true : false;
}

/*

=pod

=head1 NAME 

Perl6.MetaClass - Metaclass in the Perl 6 Meta Model

=head1 DESCRIPTION

=head1 METHODS 

=over 4

=item B<new Perl6.MetaClass (name, version, authority)>

=item B<name (?name)>

=item B<version (?version)>

=item B<authority (?authority)>

=item B<identifier>

=item B<superclasses (?superclasses)>

=item B<is_a (?classname)>

=item B<MRO>

=item B<dispatcher (?order)>

=item B<add_method (label, method)>

=item B<get_method (label)>

=item B<has_method (label)>

=back

=head1 AUTHOR

Stevan Little E<lt>stevan@iinteractive.comE<gt>

=cut

*/
