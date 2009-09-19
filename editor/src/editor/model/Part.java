package editor.model;

import java.util.ArrayList;

public class Part extends Obj
{
	char data;
	ArrayList<Obj> children;

	public Part(DocTree parent, char data)
	{
		this.parent = parent;
		this.data = data;
		this.children = new ArrayList<Obj>();		
	}

	public char getData()
	{
		return data;
	}

	public void setData(String text)
	{
		this.data = data;
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
		return "Part<" + data + ">[" + childrenText + "]";
	}

	@Override public int insertText(int pos, char c)
	{
		if (pos == 0)
		{
			Part p = new Part(parent, c);
			p.children.add(this);
			parent.replace(this, p);
			return -1;
		}
		else if (pos == 1)
		{
			children.add(0, new Part(this, c));
			return -1;
		}
		
		pos--;
		
		for (int i = 0; i < children.size(); i++)
		{
			pos = children.get(i).insertText(pos, c);
			if (pos == 0)
			{
				children.add(i+1, new Part(this, c));
				return -1;
			}
		}
		
		return pos;
	}
	
	@Override public int removeText(int pos)
	{
		if (pos == 0)
		{
			if (children.size() > 0)
			{
				Obj obj = children.get(0);
				children.remove(0);
				parent.replace(this, obj);
				((Part)obj).children.addAll(children);
			}
			else
			{
				parent.remove(this);
			}
			return -1;
		}
		else
		{
			pos--;
			for (Obj child : children)
			{
				pos = child.removeText(pos);
				if (pos < 0) 
					return -1;
			}
		}
		
		return pos;
	}

	@Override public void replace(Obj oldObj, Obj newObj)
	{
		int i = children.indexOf(oldObj);
		children.remove(i);
		children.add(i, newObj);
		newObj.parent = this;
	}

	@Override public void remove(Obj obj)
	{
		children.remove(obj);
	}
	
	@Override public int findObj(int pos, ArrayList<Obj> outObj)
	{
		if (pos == 0)
		{
			outObj.add(this);
			return -1;
		}
		for (Obj c : children)
		{
			pos = findObj(--pos, outObj);
			if (pos == -1)
				break;
		}
		
		return pos;
	}

}
