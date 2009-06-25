package editor.ui;

import java.awt.Color;
import java.util.HashMap;
import java.util.Set;

import editor.model.Dimension;
import editor.model.Dimensions;

public class ColorManager 
{
	private static Color defaultColor = new Color(255,255,255);
	HashMap<String, Color> colors = new HashMap<String,Color>();

	public Color getColor(Set<String> s)
	{
		for (String t : colors.keySet())
		{
			if (s.contains(t))
			{
				return colors.get(t);
			}
		}
		return defaultColor;
	}

	public Color getColor(String tag)
	{
		if (colors.keySet().contains(tag))
		{
			return colors.get(tag);
		}
		
		return defaultColor;
	}

	public void setDimensions(Dimensions dimensions)
	{
		//colors.clear();
		
		for (Dimension dim : dimensions)
		{
			Color c;
			
			c = getColor(dim.getTags());
			
			if (c.equals(defaultColor))
			{
				c = nextColor();
			}
			
			for (String t : dim.getTags())
			{
				colors.put(t, c);
			}
		}
	}
	
	float i = 0.0f;
	float denom = 1.0f;
	float start = 0.5f;

	private Color nextColor()
	{
		i += 1.0f;
		if (i >= denom)
		{
			denom *= 2.0f;
			start /= 2.0f;
			i = start;
		}

		float h =  i / denom;
		Color c = Color.getHSBColor(h, 0.8f, 1.0f);
		return new Color(c.getRed(), c.getBlue(), c.getGreen(), 128);
	}
}
