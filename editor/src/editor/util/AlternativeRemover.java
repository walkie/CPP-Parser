package editor.util;

import editor.AbstractVersionedObject;
import editor.Choice;
import editor.Label;
import editor.VersionedObject;

public class AlternativeRemover extends VersionedObjectTransformer
{
	int pos;
	boolean done = false;
	
	public AlternativeRemover(int pos)
	{
		this.pos = pos;
	}

	public AbstractVersionedObject transform(Choice choice)
	{
		if (done)
		{
			return super.transform(choice);
		}
		else
		{
			Choice c = new Choice();
			
			for (Label l : choice.getLabels())
			{
				AbstractVersionedObject v = choice.getAlternative(l).transform(this);
				if (pos > 0)
				{
					c.addAlternative(l, v);
				}
			}
			
			return c;
		}
	}
	
	public AbstractVersionedObject transform(VersionedObject v)
	{
		pos -= v.getValue().length();
		return super.transform(v);
	}
}
