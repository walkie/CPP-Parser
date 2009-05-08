package editor;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Set;
import java.util.TreeSet;

import editor.util.VersionedObjectTransformer;
import editor.util.VersionedObjectVisitor;

public class VersionedObject extends AbstractVersionedObject {
	String value;
	ArrayList<AbstractVersionedObject> subObjects = new ArrayList<AbstractVersionedObject>();

	public VersionedObject(String value)
	{
		this.value = value;
	}
	
	public String getValue()
	{
		return value;
	}
	
	public void addSubObject(AbstractVersionedObject v)
	{
		v.setParentObject(this);
		subObjects.add(v);
	}

	public void removeSubObject(AbstractVersionedObject o) 
	{
		subObjects.remove(o);
	}
	
	@Override
	public Set<String> tags()
	{
		TreeSet<String> ts = new TreeSet<String>();
		for (AbstractVersionedObject v : subObjects)
		{
			ts.addAll(v.tags());
		}
		
		return ts;
	}
		
	@Override
	public AbstractVersionedObject replace(Variable var, AbstractVersionedObject bound) {
		VersionedObject v = new VersionedObject(value);
		for (AbstractVersionedObject vo : subObjects)
		{
			v.subObjects.add(vo.replace(var, bound));
		}

		return v;
	}
	
	@Override
	public void visit(VersionedObjectVisitor v)
	{
		v.visit(this);
	}

	public Collection<AbstractVersionedObject> getSubObjects() {
		return subObjects;
	}

	@Override
	public AbstractVersionedObject transform(VersionedObjectTransformer v) {
		return v.transform(this);
	}
	
	@Override
	public boolean equals(Object o)
	{
		if (o instanceof VersionedObject)
		{
			VersionedObject v = (VersionedObject)o;
			return getValue().equals(v.getValue()) 
			    && getSubObjects().containsAll(v.getSubObjects()) 
			    && v.getSubObjects().containsAll(getSubObjects());
		}
		else
		{
			return false;
		}
	}
}
