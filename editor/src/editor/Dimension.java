package editor;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;
import java.util.Set;
import java.util.TreeSet;

import editor.util.ChoiceFinder;

public class Dimension {

	private AbstractVersionedObject doc;
	private ArrayList<Set<String>> dimensions;
	
	public Dimension(AbstractVersionedObject doc)
	{
		this.doc = doc;
		this.dimensions = new ArrayList<Set<String>>();
		deriveDimesions();
	}

	private void deriveDimesions()
	{
		ChoiceFinder cf = new ChoiceFinder();
		doc.visit(cf);
		Collection<Choice> cs = cf.getChoices();
		for (Choice c : cs)
		{
			dimensions.add(c.tags());
		}
		
		for (int i = 0;  i < dimensions.size(); i++)
		{
			for (int j = dimensions.size()-1; j > i; j--)
			{
				if (intersects(dimensions.get(i), dimensions.get(j)))
				{
					dimensions.get(i).addAll(dimensions.get(j));
					dimensions.remove(j);
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

	public Collection<Set<String>> getDimensions() 
	{
		return dimensions;
	}

	public TreeSet<String> getDefaults()
	{
		TreeSet<String> defaults = new TreeSet<String>();
		for (Set<String> s : dimensions)
		{
			Iterator<String> it = s.iterator();
			String t = it.next();
			defaults.add(t);
		}

		return defaults;
	}
}
