package editor.util;

import editor.AbstractVersionedObject;
import editor.Choice;
import editor.Label;
import editor.VersionedObject;

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

		VersionedObject v2 = new VersionedObject(v.getValue());
		for (AbstractVersionedObject o : v.getSubObjects())
		{
			v2.addSubObject(o.transform(this));
		}
		
		return v2;
	}
}