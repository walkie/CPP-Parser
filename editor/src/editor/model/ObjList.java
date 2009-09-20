package editor.model;

import java.util.ArrayList;

public class ObjList extends Obj
{
	final ArrayList<Obj> children;

	public ObjList(DocTree parent)
	{
		this.parent = parent;
		this.children = new ArrayList<Obj>();		
	}

	@Override public String debugGetText()
	{
		String childrenText = "";
		
		for (int i = 0; i < children.size(); i++)
		{
			childrenText += children.get(i).debugGetText();
			if (i < children.size() - 1)
				childrenText += ",";
		}
		return "[" + childrenText + "]";
	}

	@Override public int addAt(int pos, Obj obj)
	{
		if (pos == 0)
		{
			obj.parent = this;
			children.add(0, obj);
			return -1;
		}

		for (int i = 0; i < children.size(); i++)
		{
			pos = children.get(i).addAt(pos, obj);
			if (pos == 0)
			{
				obj.parent = this;
				children.add(i+1, obj);
				return -1;
			}
		}
		
		return pos;
	}

	public void addEnd(Obj obj)
	{
		obj.parent = this;
		children.add(obj);
	}
	
	@Override public int removeText(int pos)
	{
		for (Obj child : children)
		{
			pos = child.removeText(pos);
			if (pos < 0) 
				return -1;
		}
		
		return pos;
	}
	

	@Override public void remove(Obj obj)
	{
		children.remove(obj);
	}
	
	@Override public void replace(Obj oldObj, Obj newObj)
	{
		int i = children.indexOf(oldObj);
		children.remove(i);
		children.add(i, newObj);
		newObj.parent = this;
	}

	@Override public int findObj(int pos, ArrayList<Obj> outObj)
	{
		for (Obj c : children)
		{
			pos = c.findObj(pos, outObj);
			if (pos == -1)
				break;
		}
		
		return pos;
	}

	@Override public int getBetween(int pos, int start, int end, ObjList objList)
	{
		int i = 0;
		while (i < children.size())
		{
			pos = children.get(i).getBetween(pos, start, end, objList);
			if (inc)
			{
				i++;
			}
			inc = true;
		}
		
		return pos;
	}

	public void setRemove()
	{
		inc = false;
	}
	boolean inc = true;
	
	@Override public int removeBetween(int pos, int start, int end)
	{
		for (int i = children.size() - 1; i >= 0; i--)
		{
			pos = children.get(i).removeBetween(pos, start, end);
		}
		
		return pos;
	}
}
