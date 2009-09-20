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
	public int addAt(int pos, Obj obj)
	{
		if (pos == 0)
		{
			obj.parent = parent;
			parent.replace(this, obj);
			return -1;
		}
		
		return pos;
	}
}
