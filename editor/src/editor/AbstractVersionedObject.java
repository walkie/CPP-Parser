package editor;

import java.util.Set;

import editor.util.VersionedObjectTransformer;
import editor.util.VersionedObjectVisitor;

public abstract class AbstractVersionedObject {
	AbstractVersionedObject parentObject = null;
	
	public abstract Set<String> tags();
	public abstract void visit(VersionedObjectVisitor v);
	public abstract AbstractVersionedObject transform(VersionedObjectTransformer v);
	protected abstract AbstractVersionedObject replace(Variable var, AbstractVersionedObject bound);
	public abstract AbstractVersionedObject copy();
	
	public AbstractVersionedObject getParentObject() 
	{
		return parentObject;
	}
	
	public void setParentObject(AbstractVersionedObject parentObject)
	{
		this.parentObject = parentObject;
	}
}
