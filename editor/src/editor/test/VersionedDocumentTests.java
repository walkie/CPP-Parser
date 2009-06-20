package editor.test;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

import org.junit.Before;
import org.junit.Test;

import editor.controller.VersionedDocument;
import editor.model.Document;
import editor.model.VersionedObject;

public class VersionedDocumentTests
{
	VersionedDocument docWithJustText = null;
//	VersionedDocument docWithSingleAltChoice = null;
//	VersionedDocument docWithTwoAltChoice = null;
//	VersionedDocument docWithTwoDimesnions = null;
//	
	@Before public void setUp()
	{
		VersionedObject v = new VersionedObject(new Document(), "ToSC");
		docWithJustText = new VersionedDocument(v);
	}
//		ChoiceOld c = new ChoiceOld();
//		v = new VersionedObjectOld("ToSC");
//		c.addAlternative(new LabelOld("t"), v);
//		docWithSingleAltChoice = new VersionedDocument(c);
//
//		c = new ChoiceOld();
//		v = new VersionedObjectOld("ToSC1");
//		c.addAlternative(new LabelOld("t"), v);
//		v = new VersionedObjectOld("ToSC2");
//		c.addAlternative(new LabelOld("u"), v);
//		docWithTwoAltChoice = new VersionedDocument(c);
//
//		VersionedObjectOld v1 = new VersionedObjectOld("");
//		c = new ChoiceOld();
//		v = new VersionedObjectOld("ToSC1");
//		c.addAlternative(new LabelOld("t"), v);
//		v = new VersionedObjectOld("ToSC2");
//		c.addAlternative(new LabelOld("u"), v);
//		v1.addSubObject(c);
//		c = new ChoiceOld();
//		v = new VersionedObjectOld("ToSCA");
//		c.addAlternative(new LabelOld("v"), v);
//		v = new VersionedObjectOld("ToSCB");
//		c.addAlternative(new LabelOld("w"), v);
//		v1.addSubObject(c);
//		docWithTwoDimesnions = new VersionedDocument(v1);
//	}
//
////	public String[] getTextWithHidden(int pos) 
//	@Test public void testGetTextWithHidden() 
//	{
//	}
//
////	public Dimensions getDimensions() 
//	@Test public void testGetDimensionsTextOnly() 
//	{
//		assertEquals(0, docWithJustText.getDimensions().size());
//	}
//	@Test public void testGetDimensionsSingleAltChoice() 
//	{
//		DimensionsOld dims = docWithSingleAltChoice.getDimensions();
//		
//		assertEquals(1, dims.size());
//		assertEquals(1, dims.get(0).tags().size());
//		assertTrue(dims.get(0).tags().contains("t"));
//	}
//	@Test public void testGetDimensionsTwoAltChoice() 
//	{
//		DimensionsOld dims = docWithTwoAltChoice.getDimensions();
//		
//		assertEquals(1, dims.size());
//		assertEquals(2, dims.get(0).tags().size());
//		assertTrue(dims.get(0).tags().contains("t"));
//		assertTrue(dims.get(0).tags().contains("u"));
//	}
//	@Test public void testGetDimensionsTwoDimensions() 
//	{
//		DimensionsOld dims = docWithTwoDimesnions.getDimensions();
//		
//		assertEquals(2, dims.size());
//		assertEquals(2, dims.get(0).tags().size());
//		assertEquals(2, dims.get(1).tags().size());
//		assertTrue(dims.get(0).tags().contains("t"));
//		assertTrue(dims.get(0).tags().contains("u"));
//		assertTrue(dims.get(1).tags().contains("v"));
//		assertTrue(dims.get(1).tags().contains("w"));
//	}
//
////	public Collection<TextPart> getTextParts() 
//	@Test public void testGetTextParts() 
//	{
//	}
//
////	public void addText(int pos, String text) 
//	@Test public void testAddText() 
//	{
//		docWithJustText.addText(0, "hello");
//		
//		String text = "";
//		for (TextPart part : docWithJustText.getTextParts())
//			text += part.getText();
//		
//		assertEquals("helloToSC", text);
//	}
//
////	public void removeText(int pos, int length) 
//	@Test public void testRemoveTextAll() 
//	{
//		docWithJustText.removeText(0, 4);
//		
//		String text = "";
//		for (TextPart part : docWithJustText.getTextParts())
//			text += part.getText();
//		
//		assertEquals("", text);
//	}
//	@Test public void testRemoveTextStart() 
//	{
//		docWithJustText.removeText(0, 2);
//		
//		String text = "";
//		for (TextPart part : docWithJustText.getTextParts())
//			text += part.getText();
//		
//		assertEquals("SC", text);
//	}
//	@Test public void testRemoveTextEnd() 
//	{
//		docWithJustText.removeText(2, 2);
//		
//		String text = "";
//		for (TextPart part : docWithJustText.getTextParts())
//			text += part.getText();
//		
//		assertEquals("To", text);
//	}
//	@Test public void testRemoveTextMiddle() 
//	{
//		docWithJustText.removeText(1, 2);
//		
//		String text = "";
//		for (TextPart part : docWithJustText.getTextParts())
//			text += part.getText();
//		
//		assertEquals("TC", text);
//	}
//
////	public void createChoice(int start, int end, String tag) 
	@Test public void testCreateChoice() 
	{
		docWithJustText.createChoice(0, 0, "t");
		
		assertEquals(1, docWithJustText.getDimensions().size());
		assertTrue(docWithJustText.getDimensions().first().getTags().contains("t"));
	}
//
////	public void removeChoice(int pos)
//	@Test public void testRemoveChoice()
//	{
//	}
//
////	public boolean addAlternative(int pos, String tag, String text)
//	@Test public void testAddAlternative()
//	{
//	}
//
////	public void removeAlternative(int pos)
//	@Test public void testRemoveAlternative()
//	{
//	}
//
////	public void select(String tag)
//	@Test public void testSelect()
//	{
//	}
//		
////	public Set<String> getSelectedTags()
//	@Test public void testGetSelectedTags()
//	{
//	}
//
////	public void addTagToDim(String newTag, String oldTag, Dimension dim)
//	@Test public void testAddTagToDim()
//	{
//	}
//
////	public void removeTagFromDim(String tag, Dimension dim)
//	@Test public void testRemoveTagFromDim()
//	{
//	}
//
////	public void removeDimension(Dimension dim)
//	@Test public void testRemoveDimension()
//	{
//	}
}
