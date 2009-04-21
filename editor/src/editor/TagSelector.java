package editor;

import java.util.TreeSet;

public class TagSelector extends VersionedObjectTransformer 
{
	private final AbstractVersionedObject doc;
	
	public TagSelector(AbstractVersionedObject doc)
	{
		this.doc = doc;
	}

	public AbstractVersionedObject select(TreeSet<String> selectedTags)
	{
		return doc;
	}

	@Override
	public AbstractVersionedObject transform(Choice choice) {
		return choice;
	}
}
