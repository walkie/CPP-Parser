package editor.model;

import java.util.Collection;
import java.util.Set;

public class Tree
{
	private AbstractVersionedObject obj;
	private TreeCollection children;
	private Tree parent = null;
	
	public Tree(AbstractVersionedObject obj)
	{
		this.obj = obj;
		setChildren(obj);
	}
	
	public void removeChild(String tag)
	{
		children.remove(tag);
	}
	
	public Choice findChoice()
	{
		if (obj == null)
			return null;
		
		if (obj instanceof Choice)
			return (Choice)obj;
		else
			return getParent().findChoice();
	}
	
	public void remove()
	{
		if (!isRoot())
			getParent().remove(this);
	}
	
	public void setObject(AbstractVersionedObject obj2)
	{
		this.children = obj2.tree.children;
		this.obj = obj2;
		this.obj.tree = this;
		for (Tree child : obj2.tree.getChildTrees())
		{
			child.parent = this;
		}
	}
	
	public void setChildren(AbstractVersionedObject c)
	{
		if (obj instanceof VersionedObject)
			children = new ListTree();
		else if(obj instanceof Choice)
			children = new HashTree();
		else if (obj instanceof Let)
		{
			// TODO let
			children = null;
		}
		else if (obj instanceof Variable)
		{
			// TODO variable
			children = null;
		}
		else
			throw new IllegalArgumentException("expecting VersionedObject or Choice");
	}

	private void remove(Tree tree)
	{
		children.remove(tree);
		
		if (children.isEmpty() && !isRoot())
		{
			
			getParent().remove();
		}
	}

	public void setParent(Tree parent)
	{
		this.parent = parent;
	}
	
	public Tree getParent()
	{
		return parent;
	}
	
	public AbstractVersionedObject getObject()
	{
		return obj;
	}
	
	public Collection<AbstractVersionedObject> getChildren()
	{
		return children.getChildren();
	}

	public void addChild(AbstractVersionedObject v)
	{
		if (children instanceof ListTree)
		{
			Tree tree = v.getTree();
			tree.setParent(this);
			((ListTree)children).add(tree);
		}
		else
		{
			throw new IllegalArgumentException("expecting ListTree");
		}
	}

	public Collection<Tree> getChildTrees()
	{
		return children.getChildTrees();
	}
	
	public void addChild(String tag, AbstractVersionedObject v)
	{
		if (children instanceof HashTree)
		{
			Tree tree = new Tree(v);
			v.setTree(tree);
			tree.setParent(this);
			((HashTree)children).add(tag, tree);
		}
		else
		{
			throw new IllegalArgumentException("expecting HashTree");
		}
	}

	public Set<String> getTags()
	{
		if (children instanceof HashTree)
		{
			return ((HashTree)children).getTags();
		}
		else
		{
			throw new IllegalArgumentException("expecting HashTree");
		}
	}

	public AbstractVersionedObject getChild(String tag)
	{
		if (children instanceof HashTree)
		{
			return ((HashTree)children).get(tag);
		}
		else
		{
			throw new IllegalArgumentException("expecting HashTree");
		}
	}

	public boolean isRoot()
	{
		return getParent() == null;
	}
}
