package editor.model;

import java.util.ArrayList;

public class Let extends Obj
{
	Ref ref;
	Obj bound;
	Obj Scope;

	@Override
	public String debugGetText()
	{
		// TODO Auto-generated method stub
		return "Let<,,>";
	}

	@Override
	public int insertText(int pos, char c)
	{
		return pos;
	}
}
