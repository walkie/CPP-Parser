package editor.util;

import editor.AbstractVersionedObject;
import editor.Choice;
import editor.Label;
import editor.VersionedObject;

public class Substituter extends VersionedObjectTransformer
{
	private AbstractVersionedObject fromObject;
	private AbstractVersionedObject toObject;
	
	public Substituter(AbstractVersionedObject fromObject, AbstractVersionedObject toObject)
	{
		this.fromObject = fromObject;
		this.toObject = toObject;
	}
	
	@Override
	public AbstractVersionedObject transform(VersionedObject versionedObject)
	{
		VersionedObject v = new VersionedObject(versionedObject.getValue());
		for (AbstractVersionedObject o : versionedObject.getSubObjects())
		{
			if (o.equals(fromObject))
			{
				v.removeSubObject(o);
				v.addSubObject(toObject);
			}
			else
			{
				v.addSubObject(o.transform(this));
			}
		}
		
		return v;
	}
	
	@Override
	public AbstractVersionedObject transform(Choice choice)
	{
		Choice c = new Choice();
		
		for (Label l : choice.getLabels())
		{
			if (choice.getAlternative(l).equals(fromObject))
			{
				c.addAlternative(l, toObject);
			}
			else
			{
				c.addAlternative(l, choice.getAlternative(l).transform(this));
			}
		}
		
		return c;
	}
}
