package editor;

import java.util.Iterator;
import java.util.TreeSet;

public class Label implements Comparable<Label> {
	public TreeSet<String> tags = new TreeSet<String>();
	
	public Label(String[] ts)
	{
		for (String t : ts)
		{
			tags.add(t);
		}
	}

	private Label() {}
	
	public Label(Label l) {
		for (String t : l.tags)
		{
			tags.add(t);
		}
	}

	@SuppressWarnings("unchecked")
	@Override
    public Object clone() throws CloneNotSupportedException {
	    Label l = new Label();
	    l.tags = (TreeSet<String>) tags.clone();
	    return l;
    }

//	@Override
//	public boolean equals(Object obj) {
//		if (!(obj instanceof Label))
//			return false;
//	
//		Label l = (Label)obj;
//		for (String t : tags)
//		{
//			if (!l.tags.contains(t))
//				return false;
//		}
//		for (String t : l.tags)
//		{
//			if (!tags.contains(t))
//				return false;
//		}
//		return true;
//	}

	@Override
	public int hashCode() {
		int hc = 0;
		for (String t : tags)
			hc ^= t.hashCode();
		
		return super.hashCode();
	}

	@Override
	public boolean equals(Object o)
	{
		if (o instanceof Label)
		{
			Label l = (Label)o;
			return compareTo(l) == 0;
		}
		return false;
	}
	
	@Override
	public int compareTo(Label l) {
		Iterator<String> i1 = tags.iterator();
		Iterator<String> i2 = l.tags.iterator();
		
		while (i1.hasNext() && i2.hasNext())
		{
			String s1 = i1.next();
			String s2 = i2.next();
			if (!s1.equals(s2))
				return s1.compareTo(s2);
		}
		
		if (i1.hasNext())
			return 1;
		
		if (i2.hasNext())
			return -1;
		
		return 0;
	}	
}
