package org.perl6.metamodel.metaclass;

import java.util.*;
import org.perl6.metamodel.*;


public class Dispatcher {
	private ArrayList mro;
	private int counter = 0;

	public Dispatcher (MetaClass metaclass) {
		this(metaclass, ":canonical");
	}
	
	public Dispatcher (MetaClass metaclass, String order) {
		try {
			if (order.equals(":canonical"))
				order = ":ascendant";
			if (order.equals(":descendant"))
				_make_descendant_dispatcher(metaclass);
			if (order.equals(":ascendant"))
				_make_ascendant_dispatcher(metaclass);
		} catch (Exception e) {} 	           // This makes me feel dirty.
	}
	
	public boolean hasNext () {
		if (counter < mro.size())
			return true;
		else
			return false;
	}
	
	public MetaClass next () {
		return (MetaClass)mro.get(counter++);
	}
	
	public void remove(){}
	
	// These could be done in the constructor, but this mirrors the other metamodel
	// implementations a little more closely.
	
	private void _make_ascendant_dispatcher (MetaClass metaclass) throws Exception {
		mro = metaclass.MRO();
	}
	
	private void _make_descendant_dispatcher (MetaClass metaclass) throws Exception {
		mro = metaclass.MRO();
		Collections.reverse(mro);
	}
}