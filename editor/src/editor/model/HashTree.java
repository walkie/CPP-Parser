package editor.model;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.Set;

public class HashTree implements TreeCollection
{
	HashMap<String,Tree> children = new HashMap<String,Tree>();

	public Collection<AbstractVersionedObject> getChildren()
	{
		ArrayList<AbstractVersionedObject> a = new ArrayList<AbstractVersionedObject>();
		for (Tree t : children.values())
			a.add(t.getObject());
		return a;
	}
	
	public void add(String tag, Tree tree)
	{
		children.put(tag, tree);
	}
	
	public void remove(String tag)
	{
		children.remove(tag);
	}

	@Override public void remove(Tree tree)
	{
		if (children.containsValue(tree))
		{
			children.values().remove(tree);
		}
	}

	public Set<String> getTags()
	{
		return children.keySet();
	}

	public AbstractVersionedObject get(String tag)
	{
		return children.get(tag).getObject();
	}
}
