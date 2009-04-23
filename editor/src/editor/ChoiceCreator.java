package editor;

public class ChoiceCreator extends VersionedObjectTransformer
{
	int pos;
	String tag;
	boolean done = false;
	
	public ChoiceCreator(int pos, String tag)
	{
		this.pos = pos;
		this.tag = tag;
	}
	
	public AbstractVersionedObject transform(VersionedObject v)
	{
		if (done)
		{
			return super.transform(v);
		}
		else
		{
			pos -= v.getValue().length();
			
			if (pos <= 0)
			{
				done = true;
				Choice c = new Choice();
				c.addAlternative(new Label(tag), new VersionedObject(v.getValue()));
				return c;
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
