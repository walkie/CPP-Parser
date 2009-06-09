package editor.util;

import editor.model.AbstractVersionedObject;
import editor.model.Choice;
import editor.model.EmptyVersionedObject;
import editor.model.Label;
import editor.model.Let;
import editor.model.Variable;
import editor.model.VersionedObject;

public class VersionedObjectTransformer 
{
	public AbstractVersionedObject transform(Choice choice)
	{
		Choice c = new Choice();
		
		for (Label l : choice.getLabels())
		{
			AbstractVersionedObject v = choice.getAlternative(l).transform(this);
			c.addAlternative(new Label(l), v);
		}
		
		return c;
	}

	public AbstractVersionedObject transform(Let let)
	{
		Variable var = (Variable)let.getVar().transform(this);
		AbstractVersionedObject bound = let.getBound().transform(this);
		AbstractVersionedObject scope = let.getScope().transform(this);
		return new Let(var, bound, scope);
	}

	public AbstractVersionedObject transform(Variable var)
	{
		return new Variable(var);
	}

	public AbstractVersionedObject transform(VersionedObject v)
	{
		VersionedObject v2 = new VersionedObject(v.getValue());
		
		for (AbstractVersionedObject o : v.getSubObjects())
		{
			v2.addSubObject(o.transform(this));
		}
		
		return v2;
	}

	public AbstractVersionedObject transform(EmptyVersionedObject e)
	{
		return e.transform(this);
	}
}
