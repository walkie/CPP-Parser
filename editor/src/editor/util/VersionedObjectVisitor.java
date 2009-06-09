package editor.util;

import editor.model.AbstractVersionedObject;
import editor.model.Choice;
import editor.model.EmptyVersionedObject;
import editor.model.Let;
import editor.model.Variable;
import editor.model.VersionedObject;

public class VersionedObjectVisitor {

	public void visit(Choice choice)
	{
		for (AbstractVersionedObject v : choice.getAlternatives())
		{
			v.visit(this);
		}
	}

	public void visit(VersionedObject versionedObject) 
	{
		for (AbstractVersionedObject v : versionedObject.getSubObjects())
		{
			v.visit(this);
		}
	}

	public void visit(Variable variable) 
	{
	}

	public void visit(Let let)
	{
		let.getBound().visit(this);
		let.getScope().visit(this);
	}

	public void visit(EmptyVersionedObject emptyVersionedObject)
	{
	}
}
