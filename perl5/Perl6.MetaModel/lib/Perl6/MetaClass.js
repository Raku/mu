
var Perl6 = {};
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
    //document.write("<HR>starting merge for ....<HR>");
    while (true) {
        // remove all empty seqences
        var nonemptyseqs = _remove_empty_seqs(seqs);
        //document.write("notemptyseq(" + nonemptyseqs + ")<BR>");
        // return the list if we have no more no-empty sequences
        if (nonemptyseqs.length == 0) {
            //document.write("returning res (" + get_types(res).join(", ") + ")<BR>");
            return res;
        }
        //else {
        //    document.write("not returning res (" + nonemptyseqs.length + ")<BR>");            
        //}
        var cand = false;
        for (var i = 0; i < nonemptyseqs.length; i++) {
            var seq = nonemptyseqs[i];
            //document.write("<- checking seq : " + seq + " -> " + i + "<BR>");
            cand = seq[0]; 
            //document.write("-> got cand " + cand + "<BR>");
            var nothead = false;
            for (var j = 0; j < nonemptyseqs.length; j++) {     
                var sub_seq = nonemptyseqs[j];
                //document.write("<- checking sub_seq : " + sub_seq + " -> " + j + "<BR>");                
                if (_in_tail(sub_seq, cand)) {
                    nothead = true;
                    //document.write("at inner break<BR>");
                    break;
                }
            }
            if (nothead == false) {
                // leave the loop with our canidate ...
                //document.write("at outer break<BR>");                
                break;
            }
            else {
                // otherwise, reject it ...
                cand = false;
            }
        }
        //document.write("... outside outer loop<BR>");        
        if (cand == false) {
            throw "Inconsistent hierarchy";
        }
        //document.write("pushing cand onto res: " + cand + "<BR>");
        res[res.length] = cand;
        // now loop through our non-empties and pop 
        // off the head if it matches our canidate
        seqs = _remove_heads_if(cand, seqs);
        //document.write("... looping again ....<BR>");
    }    
    //document.write("we should never get here<HR>");
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

Perl6.MetaClass.prototype.toString = function () {
    return "Perl6.MetaClass{" + this._name + "}";
}

/* 
Array.prototype.toString = function () { 
    var str = "[";
    for (var i = 0; i < this.length; i++) {
        str += this[i].toString() + ", ";
    }
    return str + "]";
}
*/

