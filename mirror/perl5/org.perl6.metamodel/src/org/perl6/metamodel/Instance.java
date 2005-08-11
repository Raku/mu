package org.perl6.metamodel;

import java.util.*;

public class Instance {
    private Class _class;
    private HashMap attributes;
    
    public Instance (Class _class) {
        this(_class, new HashMap());
    }
    
    public Instance (Class _class, HashMap attributes) {
        this._class = _class;
        this.attributes = attributes;
    }
    
    public MetaClass meta () {
        return this._class.meta();
    }
    
    public boolean isa (String class_name) {
        return this._class.meta().is_a(class_name);
    }
    
    public Method can (String method_name) {
        HashMap opts = new HashMap();
        opts.put("for", "INSTANCE");
        try {
            return MetaModel.WALKMETH(this._class.meta().dispatcher(":canonical"),
                                    method_name,
                                    opts); 
        } catch (Exception e) {
            return null;
        }
    }
    
    public Class _class () {
        return this._class();
    }
    
}