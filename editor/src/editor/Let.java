package editor;

import java.util.Set;
import java.util.TreeSet;
import editor.util.*;

public class Let extends AbstractVersionedObject {
	
	Variable var;
	AbstractVersionedObject bound;
	AbstractVersionedObject scope;
	
	public Let(Variable var, AbstractVersionedObject bound, AbstractVersionedObject scope)
	{
		this.var = var;
		this.bound = bound;
		this.scope = scope;
	}
	
	@Override
	public Tree<String> getText() {
		AbstractVersionedObject v = scope.replace(var, bound);
		return v.getText();
	}
	
	@Override
	public AbstractVersionedObject replace(Variable v, AbstractVersionedObject b) 
	{
		Let let = new Let(var, bound.replace(v, b), scope.replace(v, b));
		
		return let.scope.replace(let.var, let.bound);
	}

	@Override
	public AbstractVersionedObject select(String tag) {
		var = (Variable) var.select(tag);
		bound = bound.select(tag);
		scope = scope.select(tag);
		
		return this;
	}

	@Override
	public Set<String> tags() {
		Set<String> ts = new TreeSet<String>();
		ts.addAll(bound.tags());
		ts.addAll(scope.tags());
		return ts;
	}

	@Override
	public String getStructuredText() {
		String v = var.getStructuredText();
		String b = bound.getStructuredText();
		String s = scope.getStructuredText();
		return String.format("<let>\n%s%s%s</let>\n", v, b, s);
	}
	
	@Override
	public void visit(VersionedObjectVisitor v)
	{
		v.visit(this);
	}

	public AbstractVersionedObject getBound() {
		return bound;
	}

	public AbstractVersionedObject getScope() {
		return scope;
	}
}
