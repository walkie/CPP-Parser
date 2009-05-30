package editor;

import java.util.Set;
import java.util.TreeSet;

public class Dimension 
{
	private TreeSet<String> tags = new TreeSet<String>();
	private String selectedTag = null;
	
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
		if (selectedTag == null && tags.size() > 0)
			selectedTag = tags.first();
		return selectedTag;
	}

	public Set<String> tags()
	{
		return tags;
	}
}
