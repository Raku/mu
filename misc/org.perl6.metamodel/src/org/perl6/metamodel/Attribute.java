
package org.perl6.metamodel;

import java.util.*;

public class Attribute {
    
    private MetaClass associated_with;
    private String accessor_name;
    private String visibility;
    private HashMap properties;
    private String label;
    
    public Attribute (MetaClass _associated_with, String _label)
    {
        this(_associated_with, _label, new HashMap());
    }
    
    public Attribute (MetaClass _associated_with, 
                      String _label, 
                      HashMap _properties)
    {
        String _accessor_name = _label.substring(2, _label.length());
        String _visibility = "public";
        
        if (_label.indexOf(':') == 1)
            _visibility = "private";

        if(!_properties.containsKey("access"))
            _properties.put("access","ro");
        if(!_properties.containsKey("type"))
            _properties.put("type", null);
        
        this.associated_with = _associated_with;
        this.accessor_name = _accessor_name;
        this.visibility = _visibility;
        this.label = _label;
        this.properties = _properties;
    }
    
    public boolean is_ro () {
        return ( (String) properties.get("access") ).equals("ro");
    }
    
    public boolean is_rw () {
        return ( (String) properties.get("access") ).equals("rw");
    }
    
    public String type () {
        return (String) properties.get("type");
    }
    
    public String label () {
        return label;
    }
    
    public boolean is_array () {
        return label.charAt(0) == '@';
    }
    
    public boolean is_hash () {
        return label.charAt(0) == '%';
    }
            
    public MetaClass associated_with () {
        return associated_with;
    }
    
    public String accessor_name () {
        return accessor_name;
    }
    
    public boolean is_private () {
        return visibility.equals("private");
    }
    
    public boolean is_public () {
        return visibility.equals("public");
    }
    
    public java.lang.Object instantiate_container () {
        if (this.is_array()) {
            return new ArrayList();
        }
        if (this.is_hash()) {
            return new HashMap();
        }
        return null;
    }
}