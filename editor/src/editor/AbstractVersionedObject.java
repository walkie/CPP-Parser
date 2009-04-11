package editor;

import java.util.Set;
import editor.util.*;

public abstract class AbstractVersionedObject {
	public abstract Set<String> tags();
	public abstract AbstractVersionedObject select(String tag);
	public abstract Tree<String> getText();
	public abstract String getStructuredText();
	public abstract AbstractVersionedObject replace(Variable var, AbstractVersionedObject bound);
	public abstract void visit(VersionedObjectVisitor v);
}
