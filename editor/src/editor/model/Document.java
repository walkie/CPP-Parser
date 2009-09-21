package editor.model;

import java.util.ArrayList;
import java.util.Collection;

import editor.model.exceptions.NoChoiceException;
import editor.ui.Adapter;

import editor.util.Debug;
import editor.util.TextAttr;

public class Document implements DocTree
{
	Dimensions dims;
	ObjList objs;
	final Adapter adapter;
	int length = 0;
	
	public Document(Adapter adapter)
	{
		this.objs = new ObjList(this);
		this.dims = new Dimensions();
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

	public Dim createChoice(String dimName, int start, int end)
	{
		Debug.print("createChoice: " + start + " " + end);

		ObjList objList = new ObjList(null);
		
		getBetween(0, start, end, objList);

		Dim dim = dims.getDim(dimName);
		
		objs.addAt(start, objList);
		objList.createChoice(dim);
			
		Debug.print("doc: **********************\n" + debugGetText() + "\n**********************");
		
		return dim;
	}
	
	public void removeChoice(int pos)
	{
		Debug.print("removeChoice: " + pos);
		ArrayList<Obj> a = new ArrayList<Obj>();
		objs.findObj(pos, a);
		
		if (a.size() > 0)
		{
			Choice c = a.get(0).findChoice();
			c.removeChoice();
		}
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

	public Dim addAlternative(int pos) throws NoChoiceException
	{
		Choice c = getChoice(pos);
		if (c == null)
			throw new NoChoiceException();
		
		return c.addAlternative(new Part(c, ' '));
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
	
	public String newDimName()
	{
		return dims.newDimName();
	}

	public Dimensions getDimensions()
	{
		return dims;
	}

	public Collection<TextAttr> getTextParts()
	{
		ArrayList<TextAttr> attrs = new ArrayList<TextAttr>();

		objs.getAttrs(0, attrs);
		
		return attrs;
	}

	public String getText()
	{
		return objs.getText();
	}
}
