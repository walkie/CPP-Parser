package editor.model;

import java.util.Set;
import java.util.TreeSet;

public class Choice extends AbstractVersionedObject
{
	Dimension dimension;
	
	public Choice(Dimension dimension)
	{
		this.dimension = dimension;
	}
	
	public void addAlternative(String tag, AbstractVersionedObject v)
	{
		dimension.addTag(tag);
		tree.addChild(tag, v);
	}
	
	public void replace(AbstractVersionedObject v)
	{
		tree.setObj(v);
	}
	
	@Override public String getText()
	{
		String text = "{";
		boolean first = true;
		for (String tag : tree.getTags())
		{
			AbstractVersionedObject v = tree.getChild(tag);
			if (first) { first = false; } else { text += ","; }
			text += tag + ":" + v.getText();
		}
		return text + "}";
	}

	@Override public Choice findChoice()
	{
		return this;
	}

	@Override public void removeTag(String tag)
	{
		tree.removeChild(tag);
		for (AbstractVersionedObject v : tree.getChildren())
		{
			v.removeTag(tag);
		}
		
		if (tree.getChildren().size() == 0)
		{
			tree.remove();
		}
	}

	public Set<String> domain()
	{
		return tree.getTags();
	}
	
	public Set<String> ctags()
	{
		TreeSet<String> ts = new TreeSet<String>();
		for (String t : domain())
		{
			ts.add(t);
		}
		
		return ts;
	}
	
	@Override public Set<String> tags()
	{
		TreeSet<String> t = new TreeSet<String>();
		t.addAll(ctags());
		
		for (AbstractVersionedObject v : tree.getChildren())
		{
			t.addAll(v.tags());
		}
		
		return t;
	}

	public AbstractVersionedObject getAlternative(String tag)
	{
		return tree.getChild(tag);
	}

	public void removeSelectedAlternative()
	{
		String tag = dimension.getSelectedTag();
		tree.removeChild(tag);
	}

	@Override public void cloneAlternative(String newTag, String oldTag)
	{
		AbstractVersionedObject v = tree.getChild(oldTag);
		tree.addChild(newTag, v.copy());
		super.cloneAlternative(newTag, oldTag);
	}
	
	@Override public AbstractVersionedObject copy()
	{
		Choice c = new Choice(dimension);
		for (String tag : dimension.getTags())
		{
			c.addAlternative(tag, tree.getChild(tag).copy());
		}
		
		return c;
	}
}
