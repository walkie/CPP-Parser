package editor.model;

import java.util.Set;

public class Document
{
	private Dimensions dimensions;
	private Tree tree;
	
	public Document()
	{
		this.dimensions = new Dimensions();
		tree = new Tree(new VersionedObject(this, ""));
	}
	
	public void addDimension(Dimension dim)
	{
		dimensions.addDimension(dim);
	}
	
	public void removeDimension(Dimension dim)
	{
		for (String tag : dim.getTags())
		{
			tree.getObject().removeTag(tag);
		}
		dimensions.removeDimension(dim);
	}
	
	public Dimensions getDimensions()
	{
		return dimensions;
	}
	
	public AbstractVersionedObject getObj()
	{
		return tree.getObject();
	}
	
	public String getText()
	{
		return tree.getObject().getText();
	}

	public void setObj(AbstractVersionedObject obj)
	{
		tree.setObj(obj);
	}

	public void removeTag(String tag)
	{
		tree.getObject().removeTag(tag);
		dimensions.removeTag(tag);
	}

	public void select(String tag)
	{
		dimensions.select(tag);
	}

	public Set<String> getSelectedTags()
	{
		return dimensions.getSelectedTags();
	}

	public Dimension findDimension(String tag)
	{
		for (Dimension dim : dimensions)
		{
			if (dim.containsTag(tag))
				return dim;
		}
		
		Dimension dim = new Dimension();
		dim.addTag(tag);
		dimensions.add(dim);
		return dim;
	}

	public Dimension findDimension(String tag, String tag2)
	{
		// TODO Merge dimensions
		return findDimension(tag);
	}

	public void cloneAlternative(String newTag, String oldTag)
	{
		// TODO Auto-generated method stub
		tree.getObject().cloneAlternative(newTag, oldTag);
	}
}
