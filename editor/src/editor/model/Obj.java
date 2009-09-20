package editor.model;

import java.util.ArrayList;

public abstract class Obj implements DocTree
{
	protected DocTree parent = null;
	
	public abstract int addAt(int pos, Obj obj);
	public abstract String debugGetText();
	
	public void replace(Obj oldObj, Obj newObj)
	{
	}
	
	public void remove(Obj obj)
	{
	}
	
	public int removeText(int pos)
	{
		return pos;
	}

	public Dim createChoice()
	{
		Dim dim = new Dim();
		Choice c = new Choice(dim);
		parent.replace(this, c);
		c.addAlternative("???", this);
		
		return dim;
	}
	
	public Choice findChoice()
	{
		if (parent == null)
			return null;
		else
			return parent.findChoice();
	}
	
	public int findObj(int pos, ArrayList<Obj> outObj)
	{
		return pos;
	}
	
	@Override public int getBetween(int pos, int start, int end, ObjList objList)
	{
		return pos;
	}
	
	@Override public int removeBetween(int pos, int start, int end)
	{
		return pos;
	}
}
