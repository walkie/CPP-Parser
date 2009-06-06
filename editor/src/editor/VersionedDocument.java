package editor;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Set;

import editor.util.ChoiceFinder;
import editor.util.ChoiceRemover;
import editor.util.Substituter;
import editor.util.TagSelector;
import editor.util.TextPart;

public class VersionedDocument 
{
	private AbstractVersionedObject doc;
	private Dimensions dimensions;
	private Collection<TextPart> selectedParts;

	public VersionedDocument()
	{
		doc = new VersionedObject("");
		dimensions = new Dimensions(doc);
		setSelectedParts();
	}

	public void setDocument(AbstractVersionedObject doc)
	{
		this.doc = doc;
		dimensions = new Dimensions(doc);
		setSelectedParts();
	}

	public String[] getTextWithHidden(int pos) 
	{
		for (TextPart p : selectedParts)
		{
			if (pos >= p.getStartPos() && pos < p.getEndPos())
			{
				return p.getTextWithHidden();
			}
		}
		
		return null;
	}

	public Dimensions getDimensions() 
	{
		return dimensions;
	}

	public Collection<TextPart> getTextParts() 
	{
		return selectedParts;
	}

	public void addText(int pos, String text) 
	{
		TextPart found = null;
		int p = pos;
		for (TextPart part : selectedParts)
		{
			if (pos >= part.getStartPos() && pos <= part.getEndPos())
			{
				found = part;
				break;
			}
			else
			{
				p -= part.getLength();
			}
		}

		if (found != null && found.getVersionedObject() instanceof VersionedObject)
		{
			VersionedObject v = (VersionedObject)found.getVersionedObject();
			String str = v.getValue().substring(0, p) + text + v.getValue().substring(p);
			System.out.println("OLD: " + v.getValue());
			v.setValue(str);
			System.out.println("NEW: " + v.getValue());
		}
		setSelectedParts();
	}

	public void removeText(int pos, int length) 
	{
		int p = pos;
		for (TextPart part : selectedParts)
		{
			if (pos >= part.getStartPos() && pos < part.getEndPos() && part.getVersionedObject() instanceof VersionedObject)
			{
				int len = removeTextFromVersionedObject(length, p, (VersionedObject)part.getVersionedObject());
				
				if (len == length)
				{
					break;
				}
				else if (len < length)
				{
					pos += len;
					length = length - len;
				}	
			}
			
			p -= part.getLength();
		}

		setSelectedParts();
	}

	private int removeTextFromVersionedObject(int length, int pos, VersionedObject v)
	{
		int len = Math.min(length, v.getValue().length()-pos);
		
		if (pos < v.getValue().length())
		{
			String str = v.getValue().substring(0, pos) + v.getValue().substring(pos+len);
			v.setValue(str);
		}
		
		return len;
	}

	public void createChoice(int start, int end, String tag) 
	{
		VersionedObject v = getVersionedObject(start, end);
		AbstractVersionedObject parent = v.getParentObject(); 
		
		Choice c = new Choice();
		c.addAlternative(new Label(tag), v);
		
		if (parent == null)
		{
			doc = c;
		}
		else
		{
			Substituter s = new Substituter(v, c);
			doc = doc.transform(s);
		}
		
		boolean inDim = false;
		for (Dimension dim : dimensions)
		{
			if (dim.tags().contains(tag))
			{
				inDim = true;
				break;
			}
		}
		
		if (!inDim)
		{
			Dimension d = new Dimension("dim");
			d.addTag(tag);
			dimensions.add(d);
		}
		
		setSelectedParts();
	}

	public void removeChoice(int pos)
	{
		ChoiceRemover cr = new ChoiceRemover(pos);	
		doc = doc.transform(cr);
	}

	public boolean addAlternative(int pos, String tag, String text)
	{
		AbstractVersionedObject v = findVersionedObjectFromPos(pos);
		
		if (v.getParentObject() instanceof Choice)
		{
			Choice c = getParentChoice(v);
			Dimension dim = dimensions.findByTags(c.tags());

			c.addAlternative(new Label(tag), new VersionedObject(text));
			dim.addTag(tag);
			setSelectedParts();
			return true;
		}
		
		return false;
	}

	private Choice getParentChoice(AbstractVersionedObject v)
	{
		if (v.getParentObject() == null)
			return null;

		if (v.getParentObject() instanceof Choice)
		{
			return (Choice)v.getParentObject();
		}
		else
		{
			return getParentChoice(v.getParentObject());
		}
	}

	public void removeAlternative(int pos)
	{
		for (TextPart p : selectedParts)
		{
			pos -= p.getLength();
			if (pos < 0)
			{
				System.out.println(p.getText());
				AbstractVersionedObject v = p.getVersionedObject();
				if (v.getParentObject() instanceof Choice)
				{
					Choice c = (Choice)v.getParentObject();
					c.removeAlternative(v);
				}
				break;
			}
		}
		
		setSelectedParts();
	}

	public void select(String tag)
	{
		for (Dimension d : dimensions)
		{
			d.select(tag);
		}

		setSelectedParts();
	}
	
	private void setSelectedParts() 
	{
		TagSelector ts = new TagSelector(dimensions);
		doc.visit(ts);
		selectedParts = ts.getTextParts();
	}
	
	private AbstractVersionedObject findVersionedObjectFromPos(int pos)
	{
		for (TextPart part : selectedParts)
		{
			if (pos >= part.getStartPos() && pos <= part.getEndPos())
			{
				return part.getVersionedObject();
			}
		}

		return null;
	}

	private VersionedObject getVersionedObject(int start, int end)
	{
		for (TextPart part : selectedParts)
		{
			if (start >= part.getStartPos() && start < part.getEndPos())
			{
				end = Math.min(part.getEndPos(), end);

				if (start == end)
				{
					start = part.getStartPos();
					end = part.getEndPos();
				}

				start -= part.getStartPos();
				end -= part.getStartPos();
				
				ArrayList<String> ps = new ArrayList<String>();
				
				if (start > 0)
				{
					ps.add(part.getText().substring(0, start));
				}
				int idx = ps.size();
				ps.add(part.getText().substring(start, end));
				if (end < part.getEndPos()-part.getStartPos())
				{
					ps.add(part.getText().substring(end, part.getEndPos()-part.getStartPos()));
				}
				
				VersionedObject v = (VersionedObject)part.getVersionedObject();
				if (ps.size() > 1)
				{
					System.out.printf("'%s' ", part.getText());
					for (String s : ps)
						System.out.printf("'%s' ", s);
					System.out.println();
					
					v.setValue(ps.get(0));
					VersionedObject r = v;
					for (int i = 1; i < ps.size(); i++)
					{
						VersionedObject v1 = new VersionedObject(ps.get(i));
						v.addSubObject(i-1, v1);
						if (idx == i)
							r = v1;
					}
					
					return r;
				}
				
				return v;
			}
		}

		return null;
	}

	public Set<String> getSelectedTags()
	{
		return dimensions.getSelectedTags();
	}

	public void addTagToDim(String newTag, String oldTag, Dimension dim)
	{
		dim.addTag(newTag);
		ChoiceFinder cf = new ChoiceFinder();
		doc.visit(cf);
		for (Choice c : cf.getChoices())
		{
			AbstractVersionedObject v = c.getAlternative(new Label(oldTag));
			if (v != null)
			{
				c.addAlternative(new Label(newTag), v.copy());
			}
		}
	}

	public void removeTagFromDim(String tag, Dimension dim)
	{
		removeTagFromDim2(tag, dim);
		setSelectedParts();
	}

	private void removeTagFromDim2(String tag, Dimension dim)
	{
		ChoiceFinder cf = new ChoiceFinder();
		doc.visit(cf);
		for (Choice c : cf.getChoices())
		{
			c.removeAlternative(new Label(tag));
		}
		
		dim.removeTag(tag);
	}

	public void removeDimension(Dimension dim)
	{
		try
		{
			for (String tag : dim.tags())
			{
				if (tag != dim.getSelectedTag())
				{
					removeTagFromDim2(tag, dim);
				}
			}
	
			ChoiceFinder cf = new ChoiceFinder();
			doc.visit(cf);
			for (Choice c : cf.getChoices())
			{
				if (c.tags().contains(dim.getSelectedTag()))
				{
					doc = doc.transform(new Substituter(c, c.getAlternative(new Label(dim.getSelectedTag()))));
				}
			}

			dimensions.remove(dim);
		}
		catch (java.util.ConcurrentModificationException ex)
		{
			// try-catch is magic
			// TODO: remove try-catch
		}

		setSelectedParts();
	}
}
