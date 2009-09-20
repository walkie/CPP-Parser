package editor.model;

import java.util.ArrayList;

public class Dimensions extends ArrayList<Dim>
{	
	public Dim getDim(String name)
	{
		Dim dim;
		if (name != null)
		{
			for (Dim d : this)
			{
				if (d.getName().equals(name))
					return d;
			}

			dim = new Dim(name);
			add(dim);
		}
		else
		{
			dim = new Dim(newDimName());
			add(dim);
		}
		
		return dim;
	}
	
	public String newDimName()
	{
		int i = size();
		
		while (hasDim("dim" + i))
		{
			i++;			
		}
		return "dim"+i;
	}

	private boolean hasDim(String name)
	{
		for (Dim d : this)
		{
			if (d.getName().equals(name))
				return true;
		}
		
		return false;
	}
}
