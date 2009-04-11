package editor;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Set;
import java.util.TreeSet;
import editor.util.*;

public class VersionedObject extends AbstractVersionedObject {
	String text;
	ArrayList<AbstractVersionedObject> subObjects = new ArrayList<AbstractVersionedObject>();

	public VersionedObject(String text)
	{
		this.text = text;
	}
	
	public void addSubObject(AbstractVersionedObject v)
	{
		subObjects.add(v);
	}
	
	@Override
	public Set<String> tags()
	{
		TreeSet<String> ts = new TreeSet<String>();
		for (AbstractVersionedObject v : subObjects)
		{
			ts.addAll(v.tags());
		}
		
		return ts;
	}
	
	@Override
	public AbstractVersionedObject select(String tag)
	{
		for (AbstractVersionedObject v : subObjects)
		{
			v.select(tag);
		}
		return this;
	}
	
	@Override
	public Tree<String> getText()
	{
		Tree<String> t = new Tree<String>(text, true);
		for (AbstractVersionedObject v : subObjects)
		{
			t.addChild(v.getText());
		}
		return t;
	}

	@Override
	public AbstractVersionedObject replace(Variable var, AbstractVersionedObject bound) {
		VersionedObject v = new VersionedObject(text);
		for (AbstractVersionedObject vo : subObjects)
		{
			v.subObjects.add(vo.replace(var, bound));
		}

		return v;
	}

	@Override
	public String getStructuredText() {
		String subText = "";
		for (AbstractVersionedObject v : subObjects)
		{
			subText += v.getStructuredText();
		}
		String r = String.format("<object>\n<text>%s</text>\n%s</object>\n", text.replace("\n",""), subText);

		return r;
	}
	
	@Override
	public void visit(VersionedObjectVisitor v)
	{
		v.visit(this);
	}

	public Collection<AbstractVersionedObject> getSubObjects() {
		return subObjects;
	}
}
