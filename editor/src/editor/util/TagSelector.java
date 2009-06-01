package editor.util;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Stack;

import editor.AbstractVersionedObject;
import editor.Choice;
import editor.Dimensions;
import editor.Label;
import editor.VersionedObject;

public class TagSelector extends VersionedObjectVisitor
{
	int pos = 0;
	ArrayList<TextPart> parts = new ArrayList<TextPart>();
	private Dimensions dimensions;
	Stack<Label> labels = new Stack<Label>();
	boolean selected = false;
	private ArrayList<TextPart> hiddenParts = new ArrayList<TextPart>();
	
	public TagSelector(Dimensions dimensions)
	{
		this.dimensions = dimensions;
		addBoundary();
	}
	
	private void addBoundary() 
	{
		//parts.add(new BoundaryPart(pos));
		//pos++;
	}

	@Override
	public void visit(VersionedObject v)
	{
		int end = pos + v.getValue().length();
		
		Label label = null;
		if (labels.size() != 0)
			label = labels.peek();
		
		if (selected || label == null)
		{
			parts.add(new TextPart(pos, end, label, selected, v, hiddenParts));
			pos = end;
			addBoundary();
		}
		else
		{
			hiddenParts.add(new TextPart(pos, end, label, selected, v, hiddenParts));
		}
		
		for (AbstractVersionedObject o : v.getSubObjects())
		{
			o.visit(this);
		}
	}

	@Override
	public void visit(Choice choice) 
	{
		hiddenParts = new ArrayList<TextPart>();
		if (intersects(dimensions.getSelectedTags(), choice.ctags()))
		{
			for (Label l : choice.getLabels())
			{
				labels.push(l);
				if (intersects(dimensions.getSelectedTags(), l.tags))
				{
					selected = true;
					choice.getAlternative(l).visit(this);
					selected = false;
				}
				else
				{
					choice.getAlternative(l).visit(this);
				}
				labels.pop();
			}
		}
		else
		{
			for (Label l : choice.getLabels())
			{
				labels.push(l);
				choice.getAlternative(l).visit(this);
				labels.pop();
			}			
		}
		hiddenParts = null;
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
