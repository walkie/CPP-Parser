package editor.model;

import java.util.Set;
import java.util.TreeSet;

public class Dimension implements Comparable<Dimension>
{
	private String name;
	private TreeSet<String> tags;
	private String selectedTag;
	
	public Dimension()
	{
		this.name = "dim";
		this.tags = new TreeSet<String>();
		this.selectedTag = null;
	}
	
	public String getName()
	{
		return name;
	}
	
	public void setName(String name)
	{
		this.name = name;
	}
	
	public void addTag(String tag)
	{
		if (selectedTag == null)
			selectedTag = tag;
		
		tags.add(tag);
	}
	
	public void removeTag(String tag)
	{
		tags.remove(tag);
	}
	
	public boolean containsTag(String tag)
	{
		return tags.contains(tag);
	}
	
	public Set<String> getTags()
	{
		return tags;
	}

	@Override public int compareTo(Dimension dim)
	{
		return (tags.containsAll(dim.tags) && dim.tags.containsAll(tags)) ? 0 : -1;
	}

	public void select(String tag)
	{
		if (tags.contains(tag))
			selectedTag = tag;
	}

	public String getSelectedTag()
	{
		return selectedTag;
	}
}
