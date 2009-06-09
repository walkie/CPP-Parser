package editor.util;

import editor.model.AbstractVersionedObject;
import editor.model.Choice;
import editor.model.Label;
import editor.model.VersionedObject;

public class AlternativeAdder extends VersionedObjectTransformer
{
	int pos;
	String tag;
	String text;
	boolean done = false;
	
	public AlternativeAdder(int pos, String tag, String text)
	{
		this.pos = pos;
		this.tag = tag;
		this.text = text;
	}
	
	public boolean suceeded()
	{
		return done;
	}
	
	public AbstractVersionedObject transform(Choice choice)
	{
		if (done || pos <= 0)
		{
			return super.transform(choice);
		}
		else
		{
			Choice c = new Choice();
			
			for (Label l : choice.getLabels())
			{
				AbstractVersionedObject v = choice.getAlternative(l).transform(this);
				c.addAlternative(l, v);
			}
			
			if (pos <= 0)
			{
				c.addAlternative(new Label(tag), new VersionedObject(text));
				done = true;
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
