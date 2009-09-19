package editor.model;

public class Empty extends Obj
{
	public Empty(DocTree parent)
	{
		this.parent = parent;	
	}
	
	@Override public String debugGetText()
	{
		return "Empty";
	}

	@Override
	public int insertText(int pos, char c)
	{
		if (pos == 0)
		{
			parent.replace(this, new Part(parent, c));
			return -1;
		}
		
		return pos;
	}
}
