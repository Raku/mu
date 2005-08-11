package org.perl6.metamodel;

import java.util.*;

public class Class {
    private MetaClass META;
    private String name;

    public Class(String _name, HashMap _options) {
        this.META = new MetaClass("_");
        this.extract_name(_name);
        this.process_options(_options);
    }

    public MetaClass meta () {
        return this.META;
    }
    
    public boolean isa (String class_name) {
        return this.meta().is_a(class_name);
    }
    
    public Method can (String method_name) {
        HashMap opts = new HashMap();
        opts.put("for", "CLASS");
        try {
            return MetaModel.WALKMETH(this.meta().dispatcher(":canonical"), 
                                    method_name, 
                                    opts);
        } catch (Exception e) {
            return null;
        }
    }
    
    private void extract_name(String long_name) {
        String[] short_name = long_name.split("-");
        try {
            this.name = short_name[0];
            this.meta().name(short_name[0]);
            this.meta().version(short_name[1]);
            this.meta().authority(short_name[2]);
        } 
        catch (ArrayIndexOutOfBoundsException e) {} 
    }

    private void process_options (HashMap options) {
        try {   
            if (options.containsKey("is")) {
                ArrayList is = (ArrayList) options.get("is");
                ArrayList supers = new ArrayList();
                
                for (int i = 0; i < is.size(); i++) {
                    supers.add( ( (Class)is.get(i) ).meta() );
                }
                this.meta().superclasses(supers);
            }
            if (options.containsKey("does")) {
                //ArrayList does = (ArrayList) options.get("does");
            }
            
            if (options.containsKey("instance")) {
                HashMap instance = (HashMap)options.get("instance");
                if (instance.containsKey("methods"))
                    store_methods((HashMap)instance.get("methods"), "INSTANCE");
                if (instance.containsKey("attrs"))
                    store_attrs((ArrayList)instance.get("attrs"), "INSTANCE");
            }
            if (options.containsKey("class")) {
                HashMap _class = (HashMap)options.get("class");
                if (_class.containsKey("methods"))
                    store_methods((HashMap)_class.get("methods"), "CLASS");
                if (_class.containsKey("attrs"))
                    store_attrs((ArrayList)_class.get("attrs"), "CLASS");
            }   
        } catch (Exception e) {
            System.out.println(e);
        }
    }
    private void store_methods(HashMap methods, String which_table) throws Exception {
        Set keys = methods.keySet();
        Iterator iter = keys.iterator();
        while (iter.hasNext()) {
            String label = (String)iter.next();
            Method method = (Method)methods.get(label);
            this.meta().add_method(label, method, which_table);
        }
    }

    private void store_attrs(ArrayList attrs, String which_table) throws Exception {
        Iterator iter = attrs.iterator();
        String name = "";
        HashMap props = new HashMap();
        while (iter.hasNext()) {
            java.lang.Object attr = iter.next();
            if ( attr.getClass().getName().equals("java.util.ArrayList") ) {
                name = (String) ((ArrayList)attr).get(0);
                props = (HashMap) ((ArrayList)attr).get(1);
            }
            else {
                name = (String)attr;
            }
            Attribute attribute = new Attribute(this.meta(), name, props);
            this.meta().add_attribute(name, attribute, which_table);    
        }
    }
}