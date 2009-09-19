package editor.model;

import java.util.ArrayList;

import editor.ui.Adapter;

import editor.util.Debug;

public class Document implements DocTree
{
	final ArrayList<Obj> objs;
	final Adapter adapter;
	
	public Document(Adapter adapter)
	{
		this.objs = new ArrayList<Obj>();
		this.objs.add(new Empty(this));
		this.adapter = adapter;
	}

	public void removeText(int pos, int length)
	{
		while (length-- > 0)
		{
			int i, p = 0;
			do
			{
				i = objs.get(p++).removeText(pos);
			} while (i >= 0 && p < objs.size());
		}
		Debug.print("doc: **********************\n" + debugGetText() + "\n**********************");
	}

	public void insertText(int pos, String insText)
	{
		for (char c : insText.toCharArray())
		{
			int i, p = 0;
			
			do
			{
				i = objs.get(p++).insertText(pos, c);
			} while (i >= 0 && p < objs.size());
		
			pos++;
		}
		
		Debug.print("doc: **********************\n" + debugGetText() + "\n**********************");
	}

	public Dim createChoice(int start, int end)
	{
		Debug.print("createChoice: " + start + " " + end);

		Obj obj = getParts(start, end);
		Dim dim = obj.createChoice();
		
		Debug.print("doc: **********************\n" + debugGetText() + "\n**********************");
		
		return dim;
	}

	private Obj getParts(int start, int end)
	{
		return objs.get(0);
	}

	public void removeChoice(int start)
	{
		Debug.print("removeChoice: " + start);
		
	}

	public void debugPrint()
	{
		System.out.println(debugGetText());
	}

	public String debugGetText()
	{
		String debugText = "";
		
		for (Obj obj : objs)
		{
			debugText += obj.debugGetText();
		}

		return debugText;
	}

	@Override public void replace(Obj oldObj, Obj newObj)
	{
		int i = objs.indexOf(oldObj);
		objs.remove(i);
		objs.add(i, newObj);
		newObj.parent = this;
	}

	@Override public void remove(Obj obj)
	{
		int i = objs.indexOf(obj);
		objs.remove(i);
		objs.add(i, new Empty(this));
	}

	public void addAlternative(int pos)
	{
		Choice c = getChoice(pos);
		c.addAlternative("???", new Empty(c));
	}

	public void removeAlternative(int pos)
	{
		Choice c = getChoice(pos);
		c.removeSelectedAlternative();
	}

	private Choice getChoice(int pos)
	{
		int p = 0;
		ArrayList<Obj> a = new ArrayList<Obj>();
		do
		{
			objs.get(p++).findObj(pos, a);
		} while (a.size() == 0 && p < objs.size());
		
		if (a.size() == 0)
			return null;
		else
			return a.get(0).findChoice();
	}

	@Override public Choice findChoice()
	{
		return null;
	}
}
