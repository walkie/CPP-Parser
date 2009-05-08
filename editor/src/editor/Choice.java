package editor;

import java.util.Collection;
import java.util.Set;
import java.util.TreeMap;
import java.util.TreeSet;

import editor.util.VersionedObjectTransformer;
import editor.util.VersionedObjectVisitor;

public class Choice extends AbstractVersionedObject {

	TreeMap<Label,AbstractVersionedObject> alternatives = new TreeMap<Label,AbstractVersionedObject>();
	
	public void addAlternative(Label l, AbstractVersionedObject p)
	{
		p.setParentObject(this);
		alternatives.put(l,p);
	}
	
	public Collection<AbstractVersionedObject> getAlternatives()
	{
		return alternatives.values();
	}

	public AbstractVersionedObject getAlternative(Label l)
	{
		return alternatives.get(l);
	}

	public void removeAlternative(Label l) 
	{
		alternatives.remove(l);
	}

	public Set<Label> getLabels()
	{
		return alternatives.keySet();
	}
	
	public Set<AbstractVersionedObject> range()
	{
		TreeSet<AbstractVersionedObject> t = new TreeSet<AbstractVersionedObject>();
		t.addAll(alternatives.values());
		return t;
	}
	
	public Set<Label> domain()
	{
		return alternatives.keySet();
	}
	
	public Set<String> ctags()
	{
		TreeSet<String> ts = new TreeSet<String>();
		for (Label l : domain())
		{
			ts.addAll(l.tags);
		}
		
		return ts;
	}
	
	@Override
	public Set<String> tags()
	{
		TreeSet<String> t = new TreeSet<String>();
		t.addAll(ctags());
		
		for (AbstractVersionedObject v : alternatives.values())
		{
			t.addAll(v.tags());
		}
		
		return t;
	}
	
	public AbstractVersionedObject lift()
	{
		for (Label l : domain())
		{
			if (l.tags.size() == 0)
				return alternatives.get(l);
		}
		return this;
	}
	
	@Override
	public AbstractVersionedObject replace(Variable var, AbstractVersionedObject bound) {
		Choice c = new Choice();
		for (Label l : alternatives.keySet())
		{
			Label l2 = new Label(l);
			c.addAlternative(l2, alternatives.get(l).replace(var, bound));
		}
		return c;
	}

	@Override
	public void visit(VersionedObjectVisitor v)
	{
		v.visit(this);
	}

	@Override
	public AbstractVersionedObject transform(VersionedObjectTransformer v) 
	{
		return v.transform(this);
	}
}
