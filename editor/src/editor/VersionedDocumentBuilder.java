package editor;

import java.util.ArrayList;

public class VersionedDocumentBuilder {

	private final ArrayList<AbstractVersionedObject> objs;
	
	public VersionedDocumentBuilder()
	{
		objs = new ArrayList<AbstractVersionedObject>();
	}
	
	public void addObject(AbstractVersionedObject v)
	{
		objs.add(v);
	}
	
	public void createChoice()
	{
		objs.add(new Choice());
	}
	
	public void removeChoice()
	{
		objs.remove(0);
	}
	
	public void addAlternative(String tag, AbstractVersionedObject v)
	{
		Choice c = (Choice)objs.get(objs.size()-1);
		c.addAlternative(new Label(tag), v);
	}
	
	public void removeAlternative(String tag)
	{
		Choice c = (Choice)objs.get(objs.size()-1);
		c.removeAlternative(new Label(tag));
	}
	
	public AbstractVersionedObject getVersionedDocument()
	{
		VersionedObject v = new VersionedObject("");
		
		for (AbstractVersionedObject o : objs)
		{
			v.addSubObject(o);
		}
		
		return v;
	}
}
