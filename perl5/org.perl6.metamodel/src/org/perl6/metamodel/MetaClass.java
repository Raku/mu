
package org.perl6.metamodel;

import java.util.*;

public class MetaClass {
    
    // meta-info
    private String name;
    private String version; 
    private String authority;   
    
    // the guts of the metaclass
    private MetaClass[] MRO;
    private MetaClass[] superclasses;
  
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

}
