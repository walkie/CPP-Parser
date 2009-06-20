package editor.model;

import java.util.Collection;
import java.util.Set;
import java.util.TreeSet;

public class VersionedObject extends AbstractVersionedObject
{
	String value;
	Document doc;
	
	public VersionedObject(Document doc, String value)
	{
		this.doc = doc;
		this.value = value;
		setTree(new Tree(this));
	}
	
	public void addSubObject(AbstractVersionedObject v)
	{
		tree.addChild(v);
	}
	
	@Override public String getText()
	{
		String text = value;
		for (AbstractVersionedObject v : tree.getChildren())
		{
			text += v.getText();
		}
		return text;
	}
	
	public Choice createChoice(String tag)
	{
		Dimension dim = doc.findDimension(tag);
		Choice c = new Choice(dim);
		tree.setObj(c);
		c.addAlternative(tag, this);
		return c;
	}

	public Choice createChoice(String tag, String tag2, VersionedObject v)
	{
		Dimension dim = doc.findDimension(tag, tag2);
		Choice c = new Choice(dim);
		tree.setObj(c);
		c.addAlternative(tag, this);
		c.addAlternative(tag2, v);
		return c;
	}
	
	public void removeChoice()
	{
		Choice c = tree.findChoice();
		if (c != null)
		{
			c.replace(this);
		}
	}
	
	@Override public void removeTag(String tag)
	{
		for (AbstractVersionedObject v : tree.getChildren())
		{
			v.removeTag(tag);
		}
	}

	public int removeText(int start, int len)
	{
		int i1 = Math.max(0, Math.min(start, value.length()));
		int j1 = Math.max(0, Math.min(start+len, value.length()));
		value = value.substring(0,i1) + value.substring(j1);
		
		return len - (j1 - i1);
	}

	public void addText(int i, String text)
	{
		int i1 = Math.max(0, Math.min(i, value.length()));
		value = value.substring(0,i1) + text + value.substring(i1);
	}
	
	
	@Override public Set<String> tags()
	{
		TreeSet<String> ts = new TreeSet<String>();
		for (AbstractVersionedObject v : tree.getChildren())
		{
			ts.addAll(v.tags());
		}
		
		return ts;
	}

	public String getValue()
	{
		return value;
	}

	public Collection<AbstractVersionedObject> getSubObjects()
	{
		return tree.getChildren();
	}

	public VersionedObject splitInTree(int start, int end)
	{
		if (start == end || (start == 0 && end == value.length()))
		{
			return this;
		}
		else
		{
			VersionedObject newObj = new VersionedObject(doc, "");
			tree.setObj(newObj);

			if (start > 0)
			{
				VersionedObject startObj = new VersionedObject(doc, value.substring(0,start));
				newObj.addSubObject(startObj);
			}

			VersionedObject midObj = new VersionedObject(doc, value.substring(start,end)); 
			newObj.addSubObject(midObj);

			if (end < value.length())
			{
				VersionedObject endObj = new VersionedObject(doc, value.substring(end)); 
				newObj.addSubObject(endObj);
			}

			return midObj;
		}
	}

	public void removeAlternative()
	{
		Choice c = findChoice();
		if (c != null)
		{
			c.removeSelectedAlternative();
		}
	}
	
	@Override public AbstractVersionedObject copy()
	{
		VersionedObject v = new VersionedObject(doc, value);
		for (AbstractVersionedObject v1 : tree.getChildren())
		{
			v.addSubObject(v1.copy());
		}
		
		return v;
	}
}
