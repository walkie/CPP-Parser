package editor.model;

import java.util.ArrayList;

public class Ref extends Obj
{
	Obj obj;

	@Override public String debugGetText()
	{
		return "Ref<>";
	}

	@Override
	public int insertText(int pos, char c)
	{
		return pos;
	}
}
