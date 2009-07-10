package editor.util;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Stack;

import editor.model.AbstractVersionedObject;
import editor.model.Choice;
import editor.model.Document;
import editor.model.Let;
import editor.model.VersionedObject;

public class TagSelector
{
	int pos = 0;
	ArrayList<TextPart> parts = new ArrayList<TextPart>();
	Stack<String> tags = new Stack<String>();
	boolean selected = false;
	private ArrayList<TextPart> hiddenParts = new ArrayList<TextPart>();
	private final Document doc;
	
	public TagSelector(Document doc)
	{
		this.doc = doc;
		findParts(doc.getObject());
	}

	public void findParts(AbstractVersionedObject obj)
	{
		if (obj instanceof VersionedObject)
			findParts((VersionedObject)obj);
		else if (obj instanceof Choice)
			findParts((Choice)obj);
		else if (obj instanceof Let)
		{
			// TODO let
		}
		else
			throw new IllegalArgumentException("expecting Choice or VersionedObject");
	}

	public void findParts(VersionedObject v)
	{
		int end = pos + v.getValue().length();
		
		String tag = null;
		if (tags.size() != 0)
			tag = tags.peek();
		
		if (selected || tag == null)
		{
			parts.add(new TextPart(pos, end, tag, selected, v, hiddenParts));
			pos = end;
		}
		else
		{
			hiddenParts.add(new TextPart(pos, end, tag, selected, v, hiddenParts));
		}
		
		for (AbstractVersionedObject o : v.getSubObjects())
		{
			findParts(o);
		}
	}

	public void findParts(Choice choice) 
	{
		hiddenParts = new ArrayList<TextPart>();
		if (intersects(doc.getDimensions().getSelectedTags(), choice.ctags()))
		{
			for (String tag : choice.tags())
			{
				tags.push(tag);
				if (doc.getDimensions().getSelectedTags().contains(tag))
				{
					selected = true;
					findParts(choice.getAlternative(tag));
					selected = false;
				}
				else
				{
					findParts(choice.getAlternative(tag));
				}
				tags.pop();
			}
		}
		else
		{
			for (String tag : choice.tags())
			{
				tags.push(tag);
				findParts(choice.getAlternative(tag));
				tags.pop();
			}			
		}
		hiddenParts = new ArrayList<TextPart>();
	}

	private boolean intersects(Collection<String> s1, Collection<String> s2)
	{
		for (String s : s1)
		{
			if (s2.contains(s))
				return true;
		}
		
		return false;
	}
	
	public Collection<TextPart> getTextParts()
	{
		return parts;
	}
}
