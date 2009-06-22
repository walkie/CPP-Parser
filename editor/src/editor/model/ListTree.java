package editor.model;

import java.util.ArrayList;
import java.util.Collection;

public class ListTree implements TreeCollection
{
	ArrayList<Tree> children = new ArrayList<Tree>();

	public Collection<AbstractVersionedObject> getChildren()
	{
		ArrayList<AbstractVersionedObject> a = new ArrayList<AbstractVersionedObject>();
		for (Tree t : children)
			a.add(t.getObject());
		return a;
	}
	
	public void add(Tree t)
	{
		children.add(t);
	}
	
	public void remove(String tag)
	{
	}
	
	@Override public void remove(Tree tree)
	{
		children.remove(tree);
	}
	
	@Override public boolean isEmpty()
	{
		return children.isEmpty();
	}
}
