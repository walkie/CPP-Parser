package editor.util;

import editor.util.TextPart;

public class BoundaryPart extends TextPart
{
	public BoundaryPart(int start)
	{
		super(start, start+1, null, true, null, null);
	}
	
	@Override
	public String getText() 
	{
		return "\u22C5";
	}
	
	@Override
	public int getLength()
	{
		return 1;
	}
}
