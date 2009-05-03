package editor.ui;

import java.awt.Color;
import java.util.Collection;
import java.util.Hashtable;
import java.util.Set;

import editor.Label;

public class ColorManager 
{
	Hashtable<String, Color> colors = new Hashtable<String,Color>();
	
	public Color getColor(Label l)
	{
		for (String t : colors.keySet())
		{
			if (l.tags.contains(t))
			{
				return colors.get(t);
			}
		}
		return new Color(255,255,255);
	}

	public Color getColor(Set<String> d) {
		for (String t : colors.keySet())
		{
			if (d.contains(t))
			{
				return colors.get(t);
			}
		}
		return new Color(255,255,255);
	}

	public void setDimensions(Collection<Set<String>> dimensions)
	{
		int i = 0;
		colors.clear();
		for (Set<String> dim : dimensions)
		{
			i += 50;
			for (String t : dim)
			{
				colors.put(t, Color.getHSBColor(255, i, 255));
			}
		}
	}
}
