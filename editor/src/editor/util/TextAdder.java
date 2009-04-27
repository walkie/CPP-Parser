package editor.util;

import editor.AbstractVersionedObject;
import editor.VersionedObject;

public class TextAdder extends VersionedObjectTransformer
{
	int pos;
	String text;
	boolean done = false;
	
	public TextAdder(int pos, String text)
	{
		this.pos = pos;
		this.text = text;
	}
		
	public AbstractVersionedObject transform(VersionedObject v)
	{
		if (done)
		{
			return super.transform(v);
		}
		else
		{
			int p = pos;
			pos -= v.getValue().length();
			
			if (pos <= 0)
			{
				done = true;
				String str = v.getValue().substring(0, p) + text + v.getValue().substring(p);
				VersionedObject v2 = new VersionedObject(str);
				return v2;
			}
			else
			{
				VersionedObject v2 = new VersionedObject(v.getValue());
				for (AbstractVersionedObject o : v.getSubObjects())
				{
					v2.addSubObject(o.transform(this));
				}
				
				return v2;
			}
		}
	}
}
