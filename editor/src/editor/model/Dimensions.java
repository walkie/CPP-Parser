package editor.model;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Set;
import java.util.TreeSet;

import editor.util.ChoiceFinder;

public class Dimensions extends ArrayList<Dimension>
{
	private static final long serialVersionUID = 1L;

	AbstractVersionedObject doc;
	
	
	public Dimensions(AbstractVersionedObject doc)
	{
		this.doc = doc;
		deriveDimesions();
	}

	private void deriveDimesions()
	{
		ChoiceFinder cf = new ChoiceFinder();
		doc.visit(cf);
		Collection<Choice> cs = cf.getChoices();
		for (Choice c : cs)
		{
			Dimension d = new Dimension("Dim");
			d.tags().addAll(c.tags());
			add(d);
		}
		
		for (int i = 0;  i < size(); i++)
		{
			for (int j = size()-1; j > i; j--)
			{
				if (intersects(get(i).tags(), get(j).tags()))
				{
					get(i).tags().addAll(get(j).tags());
					remove(j);
				}
			}
		}
	}

	private boolean intersects(Set<String> tags1, Set<String> tags2) 
	{
		for (String t : tags2)
		{
			if (tags1.contains(t))
				return true;
		}
		return false;
	}

	public Set<String> getSelectedTags()
	{
		Set<String> selectedTags = new TreeSet<String>();
		for (Dimension d : this)
		{
			String t = d.getSelectedTag();
			selectedTags.add(t);
		}

		return selectedTags;
	}

	public Dimension findByTags(Set<String> tags)
	{
		for (Dimension dim : this)
		{
			if (dim.tags().containsAll(tags))
			{
				return dim;
			}
		}
		
		return null;
	}
}
