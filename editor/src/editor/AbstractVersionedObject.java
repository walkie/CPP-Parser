package editor;

import java.util.Set;

import editor.util.VersionedObjectTransformer;
import editor.util.VersionedObjectVisitor;

public abstract class AbstractVersionedObject {
	public abstract Set<String> tags();
	public abstract String getStructuredText();
	public abstract AbstractVersionedObject replace(Variable var, AbstractVersionedObject bound);
	public abstract void visit(VersionedObjectVisitor v);
	public abstract AbstractVersionedObject transform(VersionedObjectTransformer v);
	
	private AbstractVersionedObject parentObject = null;
	
	protected void setParentObject(AbstractVersionedObject parentObject)
	{
		this.parentObject = parentObject;
	}
	
}
