package editor.util;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Stack;

import editor.model.Choice;
import editor.model.Document;
import editor.model.Part;

public class TagSelector
{
	int pos = 0;
	ArrayList<TextAttr> parts = new ArrayList<TextAttr>();
	Stack<String> tags = new Stack<String>();
	boolean selected = false;
	private ArrayList<TextAttr> hiddenParts = new ArrayList<TextAttr>();
	private final Document doc;
	
	public TagSelector(Document doc)
	{
		this.doc = doc;
//		findParts(doc.getObject());
	}

//	public void findParts(Obj obj)
//	{
//		if (obj instanceof VersionedObject)
//			findParts((VersionedObject)obj);
//		else if (obj instanceof Choice)
//			findParts((Choice)obj);
//		else if (obj instanceof Variable)
//			findParts(((Variable)obj).getBoundObject());
//		else
//			throw new IllegalArgumentException("expecting Choice or VersionedObject");
//	}

	public void findParts(Part v)
	{
		int end = pos; // + v.getValue().length();
		
		String tag = null;
		if (tags.size() != 0)
			tag = tags.peek();
		
		if (selected || tag == null)
		{
			parts.add(new TextAttr(pos, end, tag));
			pos = end;
		}
		else
		{
			hiddenParts.add(new TextAttr(pos, end, tag));
		}
		
//		for (Obj o : v.getSubObjects())
//		{
//			findParts(o);
//		}
	}

	public void findParts(Choice choice) 
	{
//		hiddenParts = new ArrayList<TextPart>();
//		if (intersects(doc.getDimensions().getSelectedTags(), choice.ctags()))
//		{
//			for (String tag : choice.tags())
//			{
//				tags.push(tag);
//				if (doc.getDimensions().getSelectedTags().contains(tag))
//				{
//					selected = true;
//					findParts(choice.getAlternative(tag));
//					selected = false;
//				}
//				else
//				{
//					findParts(choice.getAlternative(tag));
//				}
//				tags.pop();
//			}
//		}
//		else
//		{
//			for (String tag : choice.tags())
//			{
//				tags.push(tag);
//				findParts(choice.getAlternative(tag));
//				tags.pop();
//			}			
//		}
//		hiddenParts = new ArrayList<TextPart>();
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
	
	public Collection<TextAttr> getTextParts()
	{
		return parts;
	}
}
