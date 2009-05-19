package editor.util;

import editor.AbstractVersionedObject;
import editor.Label;
import editor.VersionedObject;

public class TextPart 
{
	int start;
	int end;
	Label label;
	boolean visible;
	VersionedObject v;
			
	public TextPart(int start, int end, Label label, boolean visible, VersionedObject v)
	{
		this.start = start;
		this.end = end;
		this.label = label;
		this.visible = visible;
		this.v = v;
	}
	
	public int getStartPos() { return start; }
	public int getEndPos() { return end; }
	public String getText() { return v.getValue(); }
	public Label getLabel() { return label; }
	public boolean isAlt() { return label != null; }
	public boolean isVisible() { return visible; }
	public AbstractVersionedObject getVersionedObject() { return v; }
	public int getLength() { return v.getValue().length(); }
}
