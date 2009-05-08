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
		float i = 0.0f;
		float denom = 1.0f;
		float start = 0.5f;
		
		colors.clear();
		
		System.out.println("****************");
		for (Set<String> dim : dimensions)
		{
			i += 1.0f;
			if (i >= denom)
			{
				denom *= 2.0f;
				start /= 2.0f;
				i = start;
			}

			float h = i / denom;
			System.out.println("" + i + " " + denom + " " + start + " " + h);

			for (String t : dim)
			{
				Color c = Color.getHSBColor(h, 0.8f, 1.0f);
				colors.put(t, new Color(c.getRed(), c.getBlue(), c.getGreen(), 128));
			}
		}
	}
}
