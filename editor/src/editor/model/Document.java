package editor.model;

import java.util.ArrayList;

import editor.model.exceptions.NoChoiceException;
import editor.ui.Adapter;

import editor.util.Debug;

public class Document implements DocTree
{
	ObjList objs;
	final Adapter adapter;
	int length = 0;
	
	public Document(Adapter adapter)
	{
		this.objs = new ObjList(this);
		this.adapter = adapter;
	}

	public void removeText(int pos, int length)
	{
		while (length-- > 0)
		{
			length--;
			objs.removeText(pos);
		}
		Debug.print("doc: **********************\n" + debugGetText() + "\n**********************");
	}

	public void insertText(int pos, String insText)
	{
		for (char c : insText.toCharArray())
		{
			length++;
			objs.addAt(pos++, new Part(null, c));
		}
		
		Debug.print("doc: **********************\n" + debugGetText() + "\n**********************");
	}

	public Dim createChoice(int start, int end)
	{
		Debug.print("createChoice: " + start + " " + end);

		ObjList objList = new ObjList(null);
		
		getBetween(0, start, end, objList);
		//removeBetween(length, start, end);
		
		objs.addAt(start, objList);
		Dim dim = objList.createChoice();
			
		Debug.print("doc: **********************\n" + debugGetText() + "\n**********************");
		
		return dim;
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
		return objs.debugGetText();
	}

	@Override public void replace(Obj oldObj, Obj newObj)
	{
		objs.replace(oldObj, newObj);
	}

	@Override public void remove(Obj obj)
	{
		objs.remove(obj);
	}

	public void addAlternative(int pos) throws NoChoiceException
	{
		Choice c = getChoice(pos);
		if (c == null)
			throw new NoChoiceException();
		c.addAlternative("???", new Empty(c));
	}

	public void removeAlternative(int pos)
	{
		Choice c = getChoice(pos);
		c.removeSelectedAlternative();
	}

	private Choice getChoice(int pos)
	{
		ArrayList<Obj> a = new ArrayList<Obj>();
		objs.findObj(pos, a);
		
		if (a.size() == 0)
			return null;
		else
			return a.get(0).findChoice();
	}

	@Override public Choice findChoice()
	{
		return null;
	}

	@Override public int getBetween(int pos, int start, int end, ObjList objList)
	{
		return objs.getBetween(pos, start, end, objList);
	}

	@Override public int removeBetween(int pos, int start, int end)
	{
		return objs.removeBetween(pos, start, end);
	}
}
