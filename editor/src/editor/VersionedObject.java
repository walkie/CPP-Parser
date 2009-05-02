package editor;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Set;
import java.util.TreeSet;

import editor.util.VersionedObjectTransformer;
import editor.util.VersionedObjectVisitor;

public class VersionedObject extends AbstractVersionedObject {
	String value;
	ArrayList<AbstractVersionedObject> subObjects = new ArrayList<AbstractVersionedObject>();

	public VersionedObject(String value)
	{
		this.value = value;
	}
	
	public String getValue()
	{
		return value;
	}
	
	public void addSubObject(AbstractVersionedObject v)
	{
		subObjects.add(v);
		v.setParentObject(this);
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
	public AbstractVersionedObject replace(Variable var, AbstractVersionedObject bound) {
		VersionedObject v = new VersionedObject(value);
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
		String r = String.format("<object>\n<text>%s</text>\n%s</object>\n", value.replace("\n",""), subText);

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

	@Override
	public AbstractVersionedObject transform(VersionedObjectTransformer v) {
		return v.transform(this);
	}
}
