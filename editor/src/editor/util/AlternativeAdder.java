package editor.util;

import editor.AbstractVersionedObject;
import editor.Choice;
import editor.Label;
import editor.VersionedObject;

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
				c.addAlternative(l, v);
			}
			
			if (pos <= 0)
			{
				c.addAlternative(new Label(tag), new VersionedObject(text));
			}
			
			return c;
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
