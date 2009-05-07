package editor;

import java.util.Collection;
import java.util.Set;
import java.util.TreeSet;

import editor.util.AlternativeAdder;
import editor.util.AlternativeRemover;
import editor.util.ChoiceCreator;
import editor.util.ChoiceFinder;
import editor.util.ChoiceRemover;
import editor.util.TagSelector;
import editor.util.TextAdder;
import editor.util.TagSelector.Line;

public class VersionedDocument 
{
	private AbstractVersionedObject doc;
	private TreeSet<String> selectedTags = new TreeSet<String>();
	

	public VersionedDocument()
	{
		this.doc = new VersionedObject("");
	}
	
	public void setDocument(AbstractVersionedObject doc)
	{
		this.doc = doc;
	}

	public Choice getChoice(int caretPosition) 
	{
		ChoiceFinder cf = new ChoiceFinder();
		doc.visit(cf);
		return cf.getChoice();
	}

	public Collection<Set<String>> getDimensions() 
	{
		Dimension d = new Dimension(doc);
		return d.getDimensions();
	}

	public Collection<Line> getLines() 
	{
		TagSelector ts = new TagSelector(selectedTags);
		doc.visit(ts);
		return ts.getLines();
	}

	public void addText(int pos, String text) 
	{
		TextAdder ta = new TextAdder(pos, text);
		doc = doc.transform(ta);
	}

	public void createChoice(int pos, String tag) 
	{
		ChoiceCreator cc = new ChoiceCreator(pos, tag);	
		doc = doc.transform(cc);
	}

	public void removeChoice(int pos)
	{
		ChoiceRemover cr = new ChoiceRemover(pos);	
		doc = doc.transform(cr);
	}

	public boolean addAlternative(int pos, String tag, String text)
	{
		AlternativeAdder aa = new AlternativeAdder(pos, tag, text);	
		doc = doc.transform(aa);
		return aa.suceeded();
	}

	public void removeAlternative(int pos)
	{
		AlternativeRemover ar = new AlternativeRemover(pos);		
		doc = doc.transform(ar);
	}

	public void select(String tag)
	{
		selectedTags.add(tag);
	}
	
	public void unselect(Set<String> dim)
	{
		selectedTags.removeAll(dim);
	}

	public Collection<String> getSelectedTags() 
	{
		return selectedTags;
	}
}
