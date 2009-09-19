package editor.util;

import java.util.ArrayList;
import java.util.Collection;

import editor.model.Part;

public class TextPart 
{
	private final int start;
	private final int end;
	private final String tag;
	private final boolean visible;
	private final Part v;
	private final Collection<TextPart> hiddenParts;
	
	public TextPart(int start, int end, String tag, boolean visible, Part v, Collection<TextPart> hiddenParts)
	{
		this.start = start;
		this.end = end;
		this.tag = tag;
		this.visible = visible;
		this.v = v;
		this.hiddenParts = hiddenParts;
	}
	
	public int getStartPos() { return start; }
	public int getEndPos() { return end; }
	//public String getText() { return v.getValue(); }
	public String getTag() { return tag; }
	public boolean isAlt() { return tag != null; }
	public boolean isVisible() { return visible; }
	public Part getVersionedObject() { return v; }
	//public int getLength() { return v.getValue().length(); }

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
		if (tag != null)
			textLabel += tag + ": ";
//		textLabel += getText();
		return textLabel;
	}
}
