package editor.controller;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Set;

import editor.model.*;
import editor.util.TagSelector;
import editor.util.TextAttr;

public class VersionedDocument 
{
//	private Document doc;
//	private Collection<TextPart> selectedParts;
//
//	public VersionedDocument()
//	{
//		doc = new Document();
//		setSelectedParts();
//	}
//
//	public VersionedDocument(Obj obj)
//	{
//		doc = new Document();
//		//doc.setObject(obj);
//		setSelectedParts();
//	}
//
//	public String[] getTextWithHidden(int pos) 
//	{
//		for (TextPart p : selectedParts)
//		{
//			if (pos >= p.getStartPos() && pos < p.getEndPos())
//			{
//				return p.getTextWithHidden();
//			}
//		}
//		
//		return null;
//	}
//
//	public ArrayList<Dim> getDimensions() 
//	{
//		//return doc.getDimensions();
//		return new ArrayList<Dim>();
//	}
//
//	public Collection<TextPart> getTextParts() 
//	{
//		return selectedParts;
//	}
//
//	public void addText(int pos, String text) 
//	{
//		TextPart part = getTextPart(pos);
//		
//		if (part != null)
//		{
//			//VersionedObject v = part.getVersionedObject();
//			//v.addText(pos - part.getStartPos(), text);
//		}
//		setSelectedParts();
//	}
//
//	public void removeText(int pos, int length) 
//	{
////		int p = pos;
////		for (TextPart part : selectedParts)
////		{
////			if (pos >= part.getStartPos())
////			{
////				VersionedObject v = part.getVersionedObject();
////				int len = v.removeText(p, length);
////				
////				if (len == length)
////				{
////					break;
////				}
////				else if (len < length)
////				{
////					pos += len;
////					length = length - len;
////				}	
////			}
////			
////			p -= part.getLength();
////		}
////
////		setSelectedParts();
//	}
//
//	public void createChoice(int start, int end, String tag) 
//	{
//		TextPart part = getTextPart(start);
//		int start2 = start-part.getStartPos();
//		int end2 = end-part.getStartPos();
////		VersionedObject v = part.getVersionedObject().splitInTree(start2, end2);
////		v.createChoice(tag);
//		setSelectedParts();
//	}
//
//	public void removeChoice(int pos)
//	{
////		VersionedObject v = getTextPart(pos).getVersionedObject();
////		v.removeChoice();
//		setSelectedParts();
//	}
//
//	public boolean addAlternative(int pos, String tag, String text)
//	{
////		VersionedObject v = getTextPart(pos).getVersionedObject();
////		
////		if (v.findChoice() != null)
////		{
////			Choice c = v.findChoice();
////			c.addAlternative(tag, new VersionedObject(doc, text));
////			setSelectedParts();
////			return true;
////		}
//		
//		return false;
//	}
//
//	public void removeAlternative(int pos)
//	{
////		VersionedObject v = getTextPart(pos).getVersionedObject();
////		v.removeAlternative();
//		setSelectedParts();
//	}
//
//	public void select(String tag)
//	{
////		doc.select(tag);
//		setSelectedParts();
//	}
//	
//	private void setSelectedParts() 
//	{
//		TagSelector ts = new TagSelector(doc);
//		selectedParts = ts.getTextParts();
//	}
//
//	private TextPart getTextPart(int pos)
//	{
//		if (selectedParts.size() == 1)
//			return selectedParts.iterator().next();
//		
//		for (TextPart part : selectedParts)
//		{
//			if (pos > part.getStartPos() && pos <= part.getEndPos())
//			{
//				return part;
//			}
//		}
//
//		return null;
//	}
//
//	public Set<String> getSelectedTags()
//	{
////		return doc.getSelectedTags();
//		return null;
//	}
//
//	public void addTagToDim(String newTag, String oldTag, Dim dim)
//	{
////		dim.addTag(newTag);
////		doc.cloneAlternative(newTag, oldTag);
//		setSelectedParts();
//	}
//
//	public void removeTagFromDim(String tag, Dim dim)
//	{
////		doc.removeTag(tag);
//		setSelectedParts();
//	}
//
//	public void removeDimension(Dim dim)
//	{
////		doc.removeDimension(dim);
//		setSelectedParts();
//	}
//	
//	public void debugPrint()
//	{
////		System.out.println(doc.getText());
//	}
//
//	public Document getDocument()
//	{
//		return doc;
//	}
//
//	public void setDocument(Document doc)
//	{
//		this.doc = doc;
//		setSelectedParts();
////		System.out.println(doc.getDimensions().size());
////		System.out.println(doc.getText());
//	}
}
