package editor;

import java.util.Collection;
import java.util.Set;

import editor.util.AlternativeRemover;
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
		setSelectedLines();
	}

	public void setDocument(AbstractVersionedObject doc)
	{
		this.doc = doc;
		dimensions = new Dimensions(doc);
		setSelectedLines();
	}

	public Choice getChoice(int caretPosition) 
	{
		ChoiceFinder cf = new ChoiceFinder();
		doc.visit(cf);
		return cf.getChoice();
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
		setSelectedLines();
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

		setSelectedLines();
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

	public void createChoice(int pos, String tag) 
	{
		AbstractVersionedObject v = findVersionedObjectFromPos(pos);
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
		setSelectedLines();
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
			Choice c = (Choice)v.getParentObject();
			c.addAlternative(new Label(tag), new VersionedObject(text));
			setSelectedLines();
			return true;
		}
		
		return false;
	}

	public void removeAlternative(int pos)
	{
		AlternativeRemover ar = new AlternativeRemover(pos);		
		doc = doc.transform(ar);
	}

	public void select(String tag)
	{
		for (Dimension d : dimensions)
		{
			d.select(tag);
		}

		setSelectedLines();
	}
	
	private void setSelectedLines() 
	{
		TagSelector ts = new TagSelector(dimensions);
		doc.visit(ts);
		selectedParts = ts.getTextParts();
	}

//	public Collection<String> getSelectedTags() 
//	{
//		return selectedTags;
//	}
	
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
		ChoiceFinder cf = new ChoiceFinder();
		doc.visit(cf);
		for (Choice c : cf.getChoices())
		{
			c.removeAlternative(new Label(tag));
		}
		dim.removeTag(tag);
		setSelectedLines();
	}

	public void removeDimension(Dimension dim)
	{
		dimensions.remove(dim);
		setSelectedLines();
	}
}
