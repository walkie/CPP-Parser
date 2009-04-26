package editor;

import java.util.Set;

import editor.util.VersionedObjectTransformer;
import editor.util.VersionedObjectVisitor;

public class Variable extends AbstractVersionedObject {

	private String name;
	
	public Variable(String name)
	{
		this.name = name;
	}
	
	public Variable(Variable variable)
	{
		this.name = variable.name;
	}

	@Override
	public String getText() 
	{
		return "";
	}

	@Override
	public Set<String> tags() 
	{
		return null;
	}

	@Override
	public AbstractVersionedObject replace(Variable var, AbstractVersionedObject bound)
	{
		if (equals(var))
		{
			return bound;
		}
		return var;
	}

	@Override
	public boolean equals(Object obj) 
	{
		
		if (obj instanceof Variable)
		{
			return name.equals(((Variable)obj).name);
		}
		return false;
	}

	@Override
	public String getStructuredText() 
	{
		return String.format("<var>%s</var>\n", name);
	}

	@Override
	public void visit(VersionedObjectVisitor v)
	{
		v.visit(this);
	}

	@Override
	public AbstractVersionedObject transform(VersionedObjectTransformer v) 
	{
		return v.transform(this);
	}
}