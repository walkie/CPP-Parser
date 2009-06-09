package editor;

import java.util.ArrayList;

import editor.model.*;

public class VersionedDocumentBuilder {

	private final ArrayList<AbstractVersionedObject> objs;
	
	public VersionedDocumentBuilder()
	{
		objs = new ArrayList<AbstractVersionedObject>();
	}
	
	public void createChoice(int i, String tag)
	{
		AbstractVersionedObject v = objs.get(i);
		objs.remove(i);
		Choice c = new Choice();
		c.addAlternative(new Label(tag), v);
		objs.add(i, c);
	}
	
	public void removeChoice()
	{
		objs.remove(0);
	}
	
	public void addAlternative(int i, String tag, String text)
	{
		Choice c = (Choice)objs.get(i);
		c.addAlternative(new Label(tag), new VersionedObject(text));
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

	public void addText(String text) {
		objs.add(new VersionedObject(text));
	}

	public int size() {
		return objs.size();
	}
}
