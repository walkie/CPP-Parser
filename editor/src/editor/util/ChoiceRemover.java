package editor.util;

import editor.model.AbstractVersionedObject;
import editor.model.Choice;
import editor.model.Label;
import editor.model.VersionedObject;

public class ChoiceRemover extends VersionedObjectTransformer
{
	int pos;
	boolean done = false;
	
	public ChoiceRemover(int pos)
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
				if (pos <= 0)
				{
					return v;
				}
				else
				{
					c.addAlternative(l, v);
				}
			}
			
			return choice;
		}
	}
	
	public AbstractVersionedObject transform(VersionedObject v)
	{
		pos -= v.getValue().length();
		return super.transform(v);
	}
}
