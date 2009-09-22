package editor.model;

import java.util.ArrayList;
import java.util.Collection;

import editor.util.TextAttr;

public abstract class Obj implements DocTree
{
	protected DocTree parent = null;
	
	public abstract int addAt(int pos, Obj obj);
	public abstract String debugGetText();
	public abstract int size();
	
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

	public Dim createChoice(Dim dim, boolean isNew)
	{
		Choice c = new Choice(dim);
		parent.replace(this, c);
		if (isNew)
			c.addAlternative(this);
		else
		{
			for (int i = 0; i < dim.getTags().size(); i++)
			{
				ObjList ol = new ObjList(c);
				ol.addEnd(new Part(ol, ' '));
				if (i == dim.getSelectedAltIdx())
				{
					parent = c;
					c.addAlternativeAtEnd(this);
				}
				else
				{
					c.addAlternativeAtEnd(ol);
				}
			}
		}
			
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
	
	public int getAttrs(int pos, Collection<TextAttr> attrs)
	{
		return pos;
	}
	
	public int getBetween(int pos, int start, int end, ObjList objList)
	{
		return pos;
	}
	
	public int removeBetween(int pos, int start, int end)
	{
		return pos;
	}
	
	public String getText()
	{
		return "";
	}
}
