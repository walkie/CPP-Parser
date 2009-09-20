package editor.util;

import java.util.ArrayList;
import java.util.Collection;

import editor.model.Part;

public class TextAttr
{
	private final int start;
	private final int end;
	private final String dimName;
	
	public TextAttr(int start, int end, String dimName)
	{
		this.start = start;
		this.end = end;
		this.dimName = dimName;
	}
	
	public int getStartPos() { return start; }
	public int getEndPos() { return end; }
	public String getDimName() { return dimName; }
}
