package editor;

import java.util.Set;

public abstract class AbstractVersionedObject {
	public abstract Set<String> tags();
	public abstract String getText();
	public abstract String getStructuredText();
	public abstract AbstractVersionedObject replace(Variable var, AbstractVersionedObject bound);
	public abstract void visit(VersionedObjectVisitor v);
	public abstract AbstractVersionedObject transform(VersionedObjectTransformer v);
}
