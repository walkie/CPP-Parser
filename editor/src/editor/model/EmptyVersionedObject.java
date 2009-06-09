package editor.model;

import java.util.Set;
import java.util.TreeSet;

import editor.util.VersionedObjectTransformer;
import editor.util.VersionedObjectVisitor;

public class EmptyVersionedObject extends AbstractVersionedObject
{
	@Override
	public AbstractVersionedObject copy()
	{
		return new EmptyVersionedObject();
	}

	@Override
	protected AbstractVersionedObject replace(Variable var, AbstractVersionedObject bound)
	{
		return this;
	}

	@Override
	public Set<String> tags()
	{
		return new TreeSet<String>();
	}

	@Override
	public AbstractVersionedObject transform(VersionedObjectTransformer v)
	{
		return v.transform(this);
	}

	@Override
	public void visit(VersionedObjectVisitor v)
	{
		v.visit(this);
	}
}
