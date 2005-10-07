
package org.perl6.metamodel;

import java.util.*;

public abstract class Method {

    private MetaClass associated_with;
    public static Stack CURRENT_INVOCANT_STACK;
    public static Stack CURRENT_CLASS_STACK;

    public Method (MetaClass _associated_with) {
        associated_with = _associated_with;
        CURRENT_INVOCANT_STACK = new Stack();
        CURRENT_CLASS_STACK = new Stack();
    }
/*
      We need to be able (for now) to create a free-floating method that we can pass
      to a class when we create it, since Java makes it so difficult to construct a 
      hash on the fly. If we haven't created the class yet, we have no metaclass to 
      give the method when we create it.
*/
    public Method () {
        this(null);
    }
    
    public MetaClass associated_with () {
        return associated_with;
    }

    public void associated_with (MetaClass metaclass) throws Exception {
        if (associated_with != metaclass) {
            if (associated_with == null)
                associated_with = metaclass;
            else
                throw new Exception("This method is already associated with " + associated_with.name() );
        }
    }
    public abstract java.lang.Object code (java.lang.Object inv, ArrayList args);
    
    public java.lang.Object call (java.lang.Object inv, ArrayList args) {
        CURRENT_CLASS_STACK.push(this.associated_with);
        CURRENT_INVOCANT_STACK.push(inv);
        java.lang.Object rval = code(inv, args);
        CURRENT_INVOCANT_STACK.pop();
        CURRENT_CLASS_STACK.pop();
        return rval;
    }
    
}
