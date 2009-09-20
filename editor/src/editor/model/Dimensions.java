package editor.model;

import java.util.ArrayList;

public class Dimensions
{
	ArrayList<Dim> dims = new ArrayList<Dim>();
	
	public Dim getDim(String name)
	{
		Dim dim;
		if (name != null)
		{
			for (Dim d : dims)
			{
				if (d.getName().equals(name))
					return d;
			}

			dim = new Dim(name);
			dims.add(dim);
		}
		else
		{
			dim = new Dim(newDimName());
			dims.add(dim);
		}
		
		return dim;
	}

	
	public String newDimName()
	{
		int i = dims.size();
		
		while (hasDim("dim" + i))
		{
			i++;			
		}
		return "dim"+i;
	}

	private boolean hasDim(String name)
	{
		for (Dim d : dims)
		{
			if (d.getName().equals(name))
				return true;
		}
		
		return false;
	}
}
