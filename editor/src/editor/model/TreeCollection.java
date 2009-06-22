package editor.model;

import java.util.Collection;

public interface TreeCollection
{
	Collection<AbstractVersionedObject> getChildren();
	void remove(String tag);
	void remove(Tree tree);
	boolean isEmpty();
}
