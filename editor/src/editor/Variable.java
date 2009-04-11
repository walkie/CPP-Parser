package editor;

import java.util.Set;
import editor.util.*;

public class Variable extends AbstractVersionedObject {

	private String name;
	
	public Variable(String name)
	{
		this.name = name;
	}
	
	@Override
	public Tree<String> getText() {
		return new Tree<String>("", true);
	}
	@Override
	public AbstractVersionedObject select(String tag) {
		return this;
	}

	@Override
	public Set<String> tags() {
		return null;
	}

	@Override
	public AbstractVersionedObject replace(Variable var, AbstractVersionedObject bound) {
		if (equals(var))
		{
			return bound;
		}
		return var;
	}

	@Override
	public boolean equals(Object obj) {
		
		if (obj instanceof Variable)
		{
			return name.equals(((Variable)obj).name);
		}
		return false;
	}

	@Override
	public String getStructuredText() {
		return String.format("<var>%s</var>\n", name);
	}

	@Override
	public void visit(VersionedObjectVisitor v)
	{
		v.visit(this);
	}
}
