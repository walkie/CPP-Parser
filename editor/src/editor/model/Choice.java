package editor.model;

import java.util.ArrayList;
import java.util.Collection;

import editor.util.Debug;
import editor.util.TextAttr;

public class Choice extends Obj
{
	final Dim dim;
	final ArrayList<Obj> alts;
	
	public Choice(Dim dim)
	{
		this.alts = new ArrayList<Obj>();
		this.dim = dim;
		this.dim.addChoice(this);
	}
	
	@Override public String debugGetText()
	{
		String altsText = "";
		boolean x = false;
		for (Obj alt : alts)
		{
			if (x)
			{
				altsText += ",";
			}
			altsText += alt.debugGetText();
			x = true;
		}
		return "Choice<" + altsText + ">";
	}

	@Override public int addAt(int pos, Obj obj)
	{
		Obj alt = alts.get(dim.getSelectedAltIdx());
		return alt.addAt(pos, obj);
	}

	@Override public int removeText(int pos)
	{
		Obj alt = alts.get(dim.getSelectedAltIdx());
		return alt.removeText(pos);
	}

	public Dim addAlternative(Obj obj)
	{
		int i = dim.addAlternative();
		obj.parent = this;
		alts.remove(i);
		alts.add(i, obj);		
		
		return dim;
	}

	public void addAlternativeAtEnd(Obj obj)
	{
		alts.add(obj);		
	}
	
	public void replace(Obj oldObj, Obj newObj)
	{
		newObj.parent = this;
		alts.remove(dim.getSelectedAltIdx());
		alts.add(0, newObj);		
	}

	public void remove(Obj obj)
	{
		alts.remove(dim.getSelectedAltIdx());
		alts.add(0, new Empty(this));		
	}

	public void removeSelectedAlternative()
	{
		dim.removeSelectedAlternatives();
	}

	public void remove(int selectedAltIdx)
	{
		alts.remove(selectedAltIdx);
		
		if (alts.size() == 0)
		{
			parent.remove(this);
		}	
	}
	
	public Choice findChoice()
	{
		return this;
	}
	
	@Override public int findObj(int pos, ArrayList<Obj> outObj)
	{
		return alts.get(dim.getSelectedAltIdx()).findObj(pos, outObj);
	}

	public int getBetween(int pos, int start, int end, ObjList objList)
	{
		return alts.get(dim.getSelectedAltIdx()).getBetween(pos, start, end, objList);
	}

	public int removeBetween(int pos, int start, int end)
	{
		return alts.get(dim.getSelectedAltIdx()).removeBetween(pos, start, end);
	}

	@Override public int size()
	{
		return alts.get(dim.getSelectedAltIdx()).size();
	}
	
	@Override public int getAttrs(int pos, Collection<TextAttr> attrs)
	{
		int end = pos+size();
		TextAttr attr = new TextAttr(pos, end, dim.getName());
		
		Debug.print("Attr: " + pos + " " + end + " " + dim.getName());
		
		attrs.add(attr);
		
		return end;
	}

	public void removeChoice()
	{
		Obj obj = alts.get(dim.getSelectedAltIdx());
		parent.replace(this, obj);
	}
	
	@Override public String getText()
	{
		return alts.get(dim.getSelectedAltIdx()).getText();
	}
}
