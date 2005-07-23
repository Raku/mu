
package org.perl6.metamodel;

import java.util.*;

public abstract class Method {

    private MetaClass associated_with;
    
    public Method (MetaClass _associated_with) {
        associated_with = _associated_with;
    }
    
    public MetaClass associated_with () {
        return associated_with;
    }

	public abstract Object code (Object inv, ArrayList args);
	
	public Object call (Object inv, ArrayList args) {
	    return code(inv, args);
	}
	
}