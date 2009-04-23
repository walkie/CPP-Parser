package editor.util;

import editor.AbstractVersionedObject;
import editor.Choice;
import editor.Let;
import editor.Variable;
import editor.VersionedObject;

public class VersionedObjectVisitor {

	public void visit(Choice choice)
	{
		for (AbstractVersionedObject v : choice.getAlternatives())
		{
			v.visit(this);
		}
	}

	public void visit(VersionedObject versionedObject) {
		for (AbstractVersionedObject v : versionedObject.getSubObjects())
		{
			v.visit(this);
		}
	}

	public void visit(Variable variable) {
		
	}

	public void visit(Let let) {
		let.getBound().visit(this);
		let.getScope().visit(this);
	}
}
