package editor;

import java.util.Set;
import java.util.TreeSet;

public class Dimension 
{
	private String name;
	private TreeSet<String> tags = new TreeSet<String>();
	private String selectedTag = null;
	
	public Dimension(String name)
	{
		this.name = name;
		this.tags = new TreeSet<String>();
		this.selectedTag = null;
	}
	
	public Dimension(Dimension dim)
	{
		this.name = dim.name;
		this.tags = new TreeSet<String>();
		this.tags.addAll(dim.tags);
		this.selectedTag = dim.selectedTag;
	}
	
	public void addTag(String tag)
	{
		tags.add(tag);
	}
	
	public void removeTag(String tag)
	{
		tags.remove(tag);
	}
	
	public void select(String tag)
	{
		if (tags.contains(tag))
			selectedTag = tag;
	}
	
	public String getSelectedTag()
	{
		if (!hasSelectedTag() && tags.size() > 0)
			selectedTag = tags.first();
		
		return selectedTag;
	}

	private boolean hasSelectedTag()
	{
		return selectedTag != null && tags.contains(selectedTag);
	}

	public Set<String> tags()
	{
		return tags;
	}

	public String getName()
	{
		return name;
	}
}
