package editor.util;

import java.util.TreeSet;

import editor.AbstractVersionedObject;
import editor.Choice;
import editor.Label;

public class TagSelector extends VersionedObjectTransformer 
{
	private final AbstractVersionedObject doc;
	private TreeSet<String> selectedTags = null;
	
	public TagSelector(AbstractVersionedObject doc)
	{
		this.doc = doc;
	}

	public AbstractVersionedObject select(TreeSet<String> selectedTags)
	{
		this.selectedTags = selectedTags;
		return doc.transform(this);
	}

	@Override
	public AbstractVersionedObject transform(Choice choice) {
		Choice c = new Choice();
		
		for (String t : selectedTags)
		{
			if (choice.ctags().contains(t))
			{
				for (Label l : choice.getLabels())
				{
					if (l.tags.contains(t))
					{
						Label l2 = new Label(l);
						l2.tags.remove(t);
						AbstractVersionedObject o = choice.getAlternative(l).transform(this);
						c.addAlternative(l2, o);
					}
				}
			}
			else
			{
				for (Label l : choice.getLabels())
				{
					AbstractVersionedObject o = choice.getAlternative(l).transform(this);
					c.addAlternative(new Label(l), o);				
				}			
			}
		}
		
		return c.lift();
	}
}
