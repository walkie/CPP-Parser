package editor.util;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Stack;

import editor.AbstractVersionedObject;
import editor.Choice;
import editor.Label;
import editor.VersionedObject;

public class TagSelector extends VersionedObjectVisitor
{
	int pos = 0;
	ArrayList<Line> lines = new ArrayList<Line>();
	private Collection<String> selectedTags;
	Stack<Label> labels = new Stack<Label>();
	boolean selected = false;
	
	public TagSelector(Collection<String> collection)
	{
		this.selectedTags = collection;
	}
	
	@Override
	public void visit(VersionedObject v)
	{
		int end = pos + v.getValue().length();
		
		Label tag = null;
		if (labels.size() != 0)
			tag = labels.peek();
		
		if (selected || tag == null)
		{
			lines.add(new Line(pos, end, v.getValue(), tag, selected));
			pos = end;
		}
		for (AbstractVersionedObject o : v.getSubObjects())
		{
			o.visit(this);
		}
	}

	@Override
	public void visit(Choice choice) 
	{
		if (intersects(selectedTags, choice.ctags()))
		{
			for (Label l : choice.getLabels())
			{
				labels.push(l);
				if (intersects(selectedTags, l.tags))
				{
					selected = true;
					choice.getAlternative(l).visit(this);
					selected = false;
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
	
	public Collection<Line> getLines()
	{
		return lines;
	}
	
	public class Line
	{
		int start;
		int end;
		String text;
		Label label;
		boolean visible;
		
		public Line(int start, int end, String text, Label label, boolean visible)
		{
			this.start = start;
			this.end = end;
			this.text = text;
			this.label = label;
			this.visible = visible;
		}
		
		public int getStartPos() { return start; }
		public int getEndPos() { return end; }
		public String getText() { return text; }
		public Label getLabel() { return label; }
		public boolean isAlt() { return label != null; }
		public boolean isVisible() { return visible; }
	}
}
