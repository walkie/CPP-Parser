package editor.model;

import java.util.ArrayList;
import java.util.Collection;

import editor.util.Debug;
import editor.util.TextAttr;

public class Part extends Obj
{
	char data;

	public Part(DocTree parent, char data)
	{
		this.parent = parent;
		this.data = data;
	}

	public char getData()
	{
		return data;
	}

	public void setData(char data)
	{
		this.data = data;
	}
	
	@Override public String debugGetText()
	{
		return "Part<" + data + ">";
	}

	@Override public int addAt(int pos, Obj obj)
	{
		return --pos;
	}
	
	@Override public int removeText(int pos)
	{
		if (pos == 0)
		{
			parent.remove(this);
			return -1;
		}
		else
		{
			pos--;
		}
		
		return pos;
	}

	@Override public void replace(Obj oldObj, Obj newObj)
	{
		parent.replace(oldObj, newObj);
	}
	
	@Override public int findObj(int pos, ArrayList<Obj> outObj)
	{
		if (pos == 0)
		{
			outObj.add(this);
			return -1;
		}
		
		return --pos;
	}

	@Override public int getBetween(int pos, int start, int end, ObjList objList)
	{
		if (pos >= start && pos < end)
		{
			Debug.print("getBetween: " + data);
			parent.remove(this);
			((ObjList)parent).setRemove();
			objList.addEnd(this);
		}
		
		return ++pos;
	}
	
	@Override public int removeBetween(int pos, int start, int end)
	{
		if (pos > start && pos <= end)
		{
			Debug.print("removeBetween: " + data);
			
			parent.remove(this);
		}
		
		return --pos;
	}
	
	@Override public int size()
	{
		return 1;
	}

	@Override public int getAttrs(int pos, Collection<TextAttr> attrs)
	{
		return ++pos;
	}
	
	@Override public String getText()
	{
		return "" + data;
	}
}
