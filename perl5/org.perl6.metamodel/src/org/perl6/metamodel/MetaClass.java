
package org.perl6.metamodel;

import java.util.*;

public class MetaClass {
    
    // meta-info
    private String name;
    private String version; 
    private String authority;   
    
    // the guts of the metaclass
    private ArrayList MRO;
    private ArrayList superclasses = new ArrayList();
  
    // constructors
  
    public MetaClass (String _name) {
        this(_name, "0.0.0");
    }   
  
    public MetaClass (String _name, String _version) {
        name = _name;
        version = _version;
    }       
    
    public MetaClass (String _name, String _version, String _authority) {
        this(_name, _version);
        authority = _authority;
    }
    
    // meta info accessor/mutators
    
    public String name () {
        return name;
    }
    
    public void name (String _name) {
        name = _name;
    }    
    
    public String version () {
        return version;
    }
    
    public void version (String _version) {
        version = _version;
    }    
    
    public String authority () {
        return authority;
    }    

    public void authority (String _authority) {
        authority = _authority;
    }
    
    public String identifier () {
        String ident = name + "-" + version;    
        if (authority != null) {
            ident += "-" + authority;        
        }
        return ident;    
    }
    
    // superclasses
    
    public void superclasses (ArrayList _superclasses) {
        superclasses = _superclasses;
    }

    public ArrayList superclasses () {
        return superclasses;
    }
    
    public boolean is_a (String classname) {
        if (name == classname) return true;
        for (int i = 0; i < superclasses.size(); i++) {
            if (((MetaClass) superclasses.get(i)).is_a(classname)) return true;
        }      
        return false;
    }

    // MRO
    
    public ArrayList _remove_empty_seqs (ArrayList seqs) {
        ArrayList nonemptyseqs = new ArrayList();
        for (int i = 0; i < seqs.size(); i++) {
            if (((ArrayList) seqs.get(i)).size() != 0) { 
                nonemptyseqs.add(seqs.get(i));
            }
        }         
        return nonemptyseqs;
    }

    public boolean _in_tail (ArrayList seq, MetaClass cand) {
        if (seq.indexOf(cand) > 0) {
            return true;
        }
        return false;
    }
    
    public void _remove_heads_if (MetaClass cand, ArrayList nonemptyseqs) {
        for (int i = 0; i < nonemptyseqs.size(); i++) {
            ArrayList current = (ArrayList) nonemptyseqs.get(i);
            if (!current.isEmpty() && current.get(0) == cand) {
                current.remove(0);
            }
        }   
    }

    public ArrayList merge (ArrayList seqs) throws Exception {
        ArrayList res = new ArrayList();
        while (true) {
            // remove all empty seqences
            ArrayList nonemptyseqs = _remove_empty_seqs(seqs);
            // return the list if we have no more no-empty sequences
            if (nonemptyseqs.size() == 0) {
                return res;
            }
            MetaClass cand = null;
            for (int i = 0; i < nonemptyseqs.size(); i++) {
                ArrayList seq = (ArrayList) nonemptyseqs.get(i);
                cand = (MetaClass) seq.get(0); 
                boolean nothead = false;
                for (int j = 0; j < nonemptyseqs.size(); j++) {     
                    ArrayList sub_seq = (ArrayList) nonemptyseqs.get(j);
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
                    cand = null;
                }
            }
            if (cand == null) {
                throw new Exception("Inconsistent hierarchy");
            }
            res.add(cand);
            // now loop through our non-empties and pop 
            // off the head if it matches our canidate
            _remove_heads_if(cand, seqs);
        }    
    }
    
    public ArrayList MRO () throws Exception {
        if (MRO == null) {
            ArrayList args = new ArrayList();
            ArrayList _this = new ArrayList();
            _this.add(this);
            args.add(_this);
            for (int i = 0; i < superclasses.size(); i++) {
                args.add(((MetaClass) superclasses.get(i)).MRO());
            }        
            args.add(superclasses);
            MRO = merge(args);
        }
        return MRO;
    }    

}
