package editor.model;

import java.util.ArrayList;

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

	@Override public int insertText(int pos, char c)
	{
		Obj alt = alts.get(dim.getSelectedAltIdx());
		return alt.insertText(pos, c);
	}

	@Override public int removeText(int pos)
	{
		Obj alt = alts.get(dim.getSelectedAltIdx());
		return alt.removeText(pos);
	}

	public void addAlternative(String tag, Obj obj)
	{
		int i = dim.addAlternative(tag);
		obj.parent = this;
		alts.remove(i);
		alts.add(i, obj);		
	}
	
	@Override public void replace(Obj oldObj, Obj newObj)
	{
		newObj.parent = this;
		alts.remove(dim.getSelectedAltIdx());
		alts.add(0, newObj);		
	}

	@Override public void remove(Obj obj)
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
	
	@Override public Choice findChoice()
	{
		return this;
	}
	
	@Override public int findObj(int pos, ArrayList<Obj> outObj)
	{
		return alts.get(dim.getSelectedAltIdx()).findObj(pos, outObj);
	}

	public void addAlternative(Obj obj)
	{
		alts.add(obj);		
	}
}
