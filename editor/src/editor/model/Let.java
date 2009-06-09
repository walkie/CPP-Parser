package editor.model;

import java.util.Set;
import java.util.TreeSet;

import editor.util.VersionedObjectTransformer;
import editor.util.VersionedObjectVisitor;

public class Let extends AbstractVersionedObject {
	
	Variable var;
	AbstractVersionedObject bound;
	AbstractVersionedObject scope;
	
	public Let(Variable var, AbstractVersionedObject bound, AbstractVersionedObject scope)
	{
		this.var = var;
		this.bound = bound;
		this.scope = scope;
		
		var.setParentObject(this);
		bound.setParentObject(this);
		scope.setParentObject(this);
	}
	

	public AbstractVersionedObject getBound() 
	{
		return bound;
	}

	public AbstractVersionedObject getScope() 
	{
		return scope;
	}

	public Variable getVar() 
	{
		return var;
	}
	
	@Override
	public AbstractVersionedObject replace(Variable v, AbstractVersionedObject b) 
	{
		Let let = new Let(var, bound.replace(v, b), scope.replace(v, b));
		
		return let.scope.replace(let.var, let.bound);
	}

	@Override
	public Set<String> tags() 
	{
		Set<String> ts = new TreeSet<String>();
		ts.addAll(bound.tags());
		ts.addAll(scope.tags());
		return ts;
	}
	
	@Override
	public void visit(VersionedObjectVisitor v)
	{
		v.visit(this);
	}

	@Override
	public AbstractVersionedObject transform(VersionedObjectTransformer v)
	{
		return v.transform(this);
	}


	@Override
	public AbstractVersionedObject copy()
	{
		return new Let((Variable)var.copy(), bound.copy(), scope.copy());
	}
}
