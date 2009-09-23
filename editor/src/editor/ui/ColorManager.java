package editor.ui;

import java.awt.Color;
import java.util.HashMap;

import editor.model.Dim;
import editor.model.Dimensions;

public class ColorManager 
{
	HashMap<String, Color> colors = new HashMap<String,Color>();

	public Color getColor(String name)
	{
		if (!colors.keySet().contains(name))
		{
			colors.put(name, nextColor());
		}
		
		return colors.get(name);
	}

	public void setDimensions(Dimensions dimensions)
	{
		for (Dim dim : dimensions)
		{
			getColor(dim.getName());
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

	public void changeDimName(String oldName, String newName)
	{
		Color color = colors.get(oldName);
		colors.remove(oldName);
		colors.put(newName, color);
	}
}
