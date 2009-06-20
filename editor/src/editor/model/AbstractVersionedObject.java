package editor.model;

import java.util.Set;

public abstract class AbstractVersionedObject
{
	Tree tree;

	public void setTree(Tree tree)
	{
		this.tree = tree;
	}
	
	public Choice findChoice()
	{
		return tree.findChoice();
	}
	
	public abstract String getText();
	public abstract void removeTag(String tag);
	public abstract Set<String> tags();
	public abstract AbstractVersionedObject copy();

	public void cloneAlternative(String newTag, String oldTag)
	{
		for (AbstractVersionedObject v : tree.getChildren())
		{
			v.cloneAlternative(newTag, oldTag);
		}
	}
}
