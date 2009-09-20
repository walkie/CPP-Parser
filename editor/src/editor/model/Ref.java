package editor.model;

import java.util.ArrayList;

public class Ref extends Obj
{
	Obj obj;

	@Override public String debugGetText()
	{
		return "Ref<>";
	}

	@Override public int addAt(int pos, Obj obj)
	{
		return pos;
	}
	
	@Override public int size()
	{
		return obj.size();
	}
}
