package editor.model;

import java.util.Set;
import java.util.TreeSet;

public class Dimensions extends TreeSet<Dimension>
{
	private static final long serialVersionUID = 1L;
	
	public Dimensions()
	{
	}
	
	public void addDimension(Dimension dim)
	{
		add(dim);
	}
	
	public void removeDimension(Dimension dim)
	{
		remove(dim);
	}

	public void removeTag(String tag)
	{
		for (Dimension dim : this)
		{
			dim.removeTag(tag);
		}
	}
	
	public void select(String tag)
	{
		for (Dimension dim : this)
		{
			dim.select(tag);
		}
	}

	public Set<String> getSelectedTags()
	{
		TreeSet<String> selectedTags = new TreeSet<String>();
		
		for (Dimension dim : this)
		{
			if (dim.getSelectedTag() != null)
				selectedTags.add(dim.getSelectedTag());
		}
		
		return selectedTags;
	}
}
