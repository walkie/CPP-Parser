package editor.util;

import java.util.ArrayList;
import java.util.Collection;

import editor.AbstractVersionedObject;
import editor.VersionedObject;

public class TextGetter extends VersionedObjectVisitor 
{
	int pos = 0;
	ArrayList<Line> lines = new ArrayList<Line>();
	
	@Override
	public void visit(VersionedObject v)
	{
		int end = pos + v.getValue().length();
		
		lines.add(new Line(pos, end, v.getValue()));
		pos = end;
		for (AbstractVersionedObject o : v.getSubObjects())
		{
			o.visit(this);
		}
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
		
		public Line(int start, int end, String text)
		{
			this.start = start;
			this.end = end;
			this.text = text;
		}
		
		public int getStartPos() { return start; }
		public int getEndPos() { return end; }
		public String getText() { return text; }
	}
}
