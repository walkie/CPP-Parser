package editor.ui;

import java.awt.Color;
import java.util.HashMap;
import java.util.Set;

import editor.model.Dimension;
import editor.model.Dimensions;
import editor.model.Label;

public class ColorManager 
{
	HashMap<String, Color> colors = new HashMap<String,Color>();

	public Color getColor(Label label)
	{
		return getColor(label.tags);
	}
	
	public Color getColor(Set<String> s)
	{
		for (String t : colors.keySet())
		{
			if (s.contains(t))
			{
				return colors.get(t);
			}
		}
		return new Color(255,255,255);
	}

	public void setDimensions(Dimensions dimensions)
	{
		float i = 0.0f;
		float denom = 1.0f;
		float start = 0.5f;
		
		colors.clear();
		
		for (Dimension dim : dimensions)
		{
			i += 1.0f;
			if (i >= denom)
			{
				denom *= 2.0f;
				start /= 2.0f;
				i = start;
			}

			float h = i / denom;

			for (String t : dim.tags())
			{
				Color c = Color.getHSBColor(h, 0.8f, 1.0f);
				colors.put(t, new Color(c.getRed(), c.getBlue(), c.getGreen(), 128));
			}
		}
	}
}
