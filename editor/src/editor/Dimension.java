package editor;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Set;

public class Dimension {

	private AbstractVersionedObject doc;
	private ArrayList<Set<String>> dimensions;
	
	public Dimension(AbstractVersionedObject doc)
	{
		this.doc = doc;
		this.dimensions = new ArrayList<Set<String>>();
		setDimesions();
	}

	private void setDimesions()
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
				if (intersect(dimensions.get(i), dimensions.get(j)))
				{
					dimensions.get(i).addAll(dimensions.get(j));
					dimensions.remove(j);
				}
			}
		}
	}

	private boolean intersect(Set<String> tags1, Set<String> tags2) {
		for (String t : tags2)
		{
			if (tags1.contains(t))
				return true;
		}
		return false;
	}

	public Collection<Set<String>> getDimensions() {
		return dimensions;
	}
}
