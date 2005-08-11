package org.perl6.metamodel;

import java.util.*;
import org.perl6.metamodel.metaclass.Dispatcher;

public abstract class MetaModel {
    
    private static Stack CURRENT_DISPATCHER = new Stack();
    
    public static java.lang.Object call_method (java.lang.Object inv, String label, ArrayList args) 
    throws Exception {
        Dispatcher dispatcher = null;
        HashMap options = new HashMap();
        HashMap context = new HashMap();
        if (inv.getClass().getName() == "org.perl6.metamodel.Instance") {
            Instance inst = (Instance) inv;
            dispatcher = inst._class().meta().dispatcher();
            options.put("for", "INSTANCE");
        }
        else if (inv.getClass().getName() == "org.perl6.metamodel.Class") {
            Class cls = (Class) inv;
            dispatcher = cls.meta().dispatcher();
            options.put("for", "CLASS"); 
        }
        else {
            throw new Exception("Unsupported invocant");
        }
        context.put("dispatcher", dispatcher);
        context.put("label", label);
        context.put("options", options);
        context.put("inv", inv);
        context.put("args", args);
        CURRENT_DISPATCHER.push(context);
        Method method = WALKMETH(dispatcher, label, options);
        java.lang.Object rval = method.call(inv, args);
        CURRENT_DISPATCHER.pop();
        return rval;
    }
    
    public static java.lang.Object next_METHOD () throws Exception {
        HashMap curr_context = (HashMap) CURRENT_DISPATCHER.peek(); 
        Method method = WALKMETH((Dispatcher)curr_context.get("dispatcher"), 
                                (String)curr_context.get("label"),
                                (HashMap)curr_context.get("options"));
        return method.call((java.lang.Object)curr_context.get("inv"), 
                          (ArrayList)curr_context.get("args"));
    }

    public static MetaClass WALKCLASS (Dispatcher dispatcher, HashMap options) {
        if (dispatcher.hasNext())
            return dispatcher.next();
        else return null;
    }
    

    public static Method WALKMETH (Dispatcher dispatcher, String method_name) 
    throws Exception {
        return WALKMETH(dispatcher, method_name, new HashMap());
    }
    
    public static Method WALKMETH (Dispatcher dispatcher, String label, HashMap options) 
    throws Exception {
        ArrayList tried = new ArrayList();
        MetaClass current = WALKCLASS(dispatcher, options);
        while(current != null) {
            if (current.has_method(label, (String)options.get("for"))) 
                return current.get_method(label, (String)options.get("for"));
            tried.add(current.name());
            current = WALKCLASS(dispatcher, options);
        }
        throw new Exception("Method " + label + " not found in: " + tried.toString());
    }
    
    public static MetaClass CLASS () throws Exception {
        if (Method.CURRENT_CLASS_STACK.size() == 0)
            throw new Exception("You cannot call $?CLASS from outside of a MetaModel method");
        return (MetaClass)Method.CURRENT_CLASS_STACK.peek();
    }

    public static Object SELF () throws Exception {
        if (Method.CURRENT_INVOCANT_STACK.size() == 0)
            throw new Exception("You cannot call $?SELF from outside of a MetaModel method");
        return (Object)Method.CURRENT_INVOCANT_STACK.peek();
    }
}