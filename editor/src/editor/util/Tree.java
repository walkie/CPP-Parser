package editor.util;

import java.util.ArrayList;

public class Tree<T> {

	private T value;
	private ArrayList<Tree<T>> children;
	private final boolean seq;
	
	public Tree(T val, boolean seq)
	{
		this.value = val;
		this.children = new ArrayList<Tree<T>>();
		this.seq = seq;
	}
	
	public boolean isSeq()
	{
		return seq;
	}
	
	public T getValue()
	{
		return value;
	}
	
	public ArrayList<Tree<T>> getChildren()
	{
		return children;
	}

	public String flatten() {
		String str = value.toString();
		for (Tree<T> t : children)
		{
			str += t.flatten();
			if (!seq)
				break;
		}
		
		return str;
	}
	
	public void addChild(Tree<T> t) {
		children.add(t);
	}
}
