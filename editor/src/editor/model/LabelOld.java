package editor.model;

import java.util.Iterator;
import java.util.TreeSet;

public class LabelOld implements Comparable<LabelOld> {
	public TreeSet<String> tags = new TreeSet<String>();
	
	public LabelOld(String[] ts)
	{
		for (String t : ts)
		{
			tags.add(t);
		}
	}

	public LabelOld(String tag) 
	{
		tags.add(tag);
	}
	
	public LabelOld(LabelOld l) 
	{
		for (String t : l.tags)
		{
			tags.add(t);
		}
	}

	@Override
	public int hashCode() 
	{
		int hc = 0;
		for (String t : tags)
			hc ^= t.hashCode();
		
		return super.hashCode();
	}

	@Override
	public boolean equals(Object o)
	{
		if (o instanceof LabelOld)
		{
			LabelOld l = (LabelOld)o;
			return compareTo(l) == 0;
		}
		return false;
	}
	
	public int compareTo(LabelOld l) 
	{
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
