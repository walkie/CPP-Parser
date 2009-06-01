package editor.util;

import java.util.ArrayList;
import java.util.Collection;

import editor.AbstractVersionedObject;
import editor.Label;
import editor.VersionedObject;

public class TextPart 
{
	private final int start;
	private final int end;
	private final Label label;
	private final boolean visible;
	private final VersionedObject v;
	private final Collection<TextPart> hiddenParts;
	
	public TextPart(int start, int end, Label label, boolean visible, VersionedObject v, Collection<TextPart> hiddenParts)
	{
		this.start = start;
		this.end = end;
		this.label = label;
		this.visible = visible;
		this.v = v;
		this.hiddenParts = hiddenParts;
	}
	
	public int getStartPos() { return start; }
	public int getEndPos() { return end; }
	public String getText() { return v.getValue(); }
	public Label getLabel() { return label; }
	public boolean isAlt() { return label != null; }
	public boolean isVisible() { return visible; }
	public AbstractVersionedObject getVersionedObject() { return v; }
	public int getLength() { return v.getValue().length(); }

	public void addHiddenPart(TextPart p)
	{
		hiddenParts.add(p);
	}
	
	public String[] getTextWithHidden()
	{
		ArrayList<String> text = new ArrayList<String>();
		text.add(toTextLabel());
		if (hiddenParts != null)
		{
			for (TextPart p : hiddenParts)
			{
				text.add(p.toTextLabel());
			}
		}
		
		return text.toArray(new String[text.size()]);
	}

	private String toTextLabel()
	{
		String textLabel = "";
		if (label != null)
			textLabel += label.tags.first() + ": ";
		textLabel += getText();
		return textLabel;
	}
}
