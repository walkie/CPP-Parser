package editor;

import java.util.Collection;
import java.util.Set;
import java.util.TreeMap;
import java.util.TreeSet;
import editor.util.*;

public class Choice extends AbstractVersionedObject {

	TreeMap<Label,AbstractVersionedObject> alternatives = new TreeMap<Label,AbstractVersionedObject>();
	
	public void addAlternative(Label l, AbstractVersionedObject p)
	{
		alternatives.put(l,p);
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
	public AbstractVersionedObject select(String tag)
	{
		Label[] ls = new Label[domain().size()];
		domain().toArray(ls);
		
		for (Label l : ls)
		{
			if (l.tags.contains(tag))
			{
				AbstractVersionedObject v = alternatives.get(l);
				v.select(tag);
			}
			else
			{
				alternatives.remove(l);
			}
		}
		for (Label l : ls)
		{
			l.tags.remove(tag);
		}
		return lift();
	}
	
	@Override
	public Tree<String> getText()
	{
		Tree<String> t = new Tree<String>("", false);
		for (Label l : alternatives.keySet())
		{
			t.addChild(alternatives.get(l).getText());
		}
		
		return t;
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
	public String getStructuredText() {
		String as = "";
		for (AbstractVersionedObject v : alternatives.values())
		{
			as += v.getStructuredText();
		}
		return String.format("<choice>%s</choice>\n", as);
	}

	@Override
	public void visit(VersionedObjectVisitor v)
	{
		v.visit(this);
	}
	
	public Collection<AbstractVersionedObject> getAlternatives() {
		return alternatives.values();
	}
}
