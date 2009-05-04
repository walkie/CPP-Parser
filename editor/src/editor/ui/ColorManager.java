package editor.ui;

import java.awt.Color;
import java.util.Collection;
import java.util.HashMap;
import java.util.Set;

import editor.Label;

public class ColorManager 
{
	HashMap<String, Color> colors = new HashMap<String,Color>();
	
	public Color getColor(Label l)
	{
		return getColor(l.tags);
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
				colors.put(t, new Color(128, 255 - i, 128, 128));
			}
		}
	}
}
