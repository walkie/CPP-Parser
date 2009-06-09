package editor.test;

import java.util.Collection;

import editor.model.Label;
import editor.controller.VersionedDocument;
import editor.model.VersionedObject;
import editor.util.TextPart;

import static org.junit.Assert.*;

import org.junit.Test;

public class VersionedObjectTest {
	
	@Test public void testVersionedObjectTags() 
	{
		VersionedObject o = new VersionedObject("foo");
		
		assertNotNull(o.tags());
	}
	
	@Test public void testAddTextToEmptyDocument() 
	{
		VersionedDocument doc = new VersionedDocument();
		doc.addText(0, "foo");
		
		Collection<TextPart> parts = doc.getTextParts();
		
		assertEquals(1, doc.getTextParts().size());
		
		String actual = "";
		for (TextPart p : parts)
		{
			actual += p.getText();
		}
		
		assertEquals("foo", actual);
	}
	
	@Test public void testAddTextToStartOfDocument() 
	{
		VersionedDocument doc = new VersionedDocument();
		doc.addText(0, "bar");
		doc.addText(0, "foo");
		
		Collection<TextPart> parts = doc.getTextParts();
		
		assertEquals(1, doc.getTextParts().size());
		
		String actual = "";
		for (TextPart p : parts)
		{
			actual += p.getText();
		}
		
		assertEquals("foobar", actual);
	}
	@Test public void testAddTextToEndOfDocument()
	{
		VersionedDocument doc = new VersionedDocument();
		doc.addText(0, "foo");
		doc.addText(3, "bar");
		
		Collection<TextPart> parts = doc.getTextParts();
		
		assertEquals(1, doc.getTextParts().size());
		
		String actual = "";
		for (TextPart p : parts)
		{
			actual += p.getText();
		}
		
		assertEquals("foobar", actual);
	}
	@Test public void testAddTextToMiddleOfDocument()
	{
		VersionedDocument doc = new VersionedDocument();
		doc.addText(0, "foobaz");
		doc.addText(3, "bar");
		
		Collection<TextPart> parts = doc.getTextParts();
		
		assertEquals(1, doc.getTextParts().size());
		
		String actual = "";
		for (TextPart p : parts)
		{
			actual += p.getText();
		}
		
		assertEquals("foobarbaz", actual);
	}
	@Test public void removeAllTextFromDocument()
	{
		VersionedDocument doc = new VersionedDocument();
		doc.addText(0, "foobaz");
		doc.removeText(0, 6);
		
		Collection<TextPart> parts = doc.getTextParts();
		
		assertEquals(1, doc.getTextParts().size());
		
		String actual = "";
		for (TextPart p : parts)
		{
			actual += p.getText();
		}
		
		assertEquals("", actual);
	}
	@Test public void removeTextFromMiddleOfDocument()
	{
		VersionedDocument doc = new VersionedDocument();
		doc.addText(0, "foobaz");
		doc.removeText(2, 2);
		
		Collection<TextPart> parts = doc.getTextParts();
		
		assertEquals(1, doc.getTextParts().size());
		
		String actual = "";
		for (TextPart p : parts)
		{
			actual += p.getText();
		}
		
		assertEquals("foaz", actual);
	}
	@Test public void removeTextFromStartOfDocument()
	{
		VersionedDocument doc = new VersionedDocument();
		doc.addText(0, "foobaz");
		doc.removeText(0, 3);
		
		Collection<TextPart> parts = doc.getTextParts();
		
		assertEquals(1, doc.getTextParts().size());
		
		String actual = "";
		for (TextPart p : parts)
		{
			actual += p.getText();
		}
		
		assertEquals("baz", actual);
	}
	@Test public void removeTextFromEndOfDocument()
	{
		VersionedDocument doc = new VersionedDocument();
		doc.addText(0, "foobaz");
		doc.removeText(3, 3);
		
		Collection<TextPart> parts = doc.getTextParts();
		
		assertEquals(1, doc.getTextParts().size());
		
		String actual = "";
		for (TextPart p : parts)
		{
			actual += p.getText();
		}
		
		assertEquals("foo", actual);
	}
	@Test public void removeTextZeroChars()
	{
		VersionedDocument doc = new VersionedDocument();
		doc.removeText(0, 10);
		
		Collection<TextPart> parts = doc.getTextParts();
		
		assertEquals(1, doc.getTextParts().size());
		
		String actual = "";
		for (TextPart p : parts)
		{
			actual += p.getText();
		}
		
		assertEquals("", actual);
	}
	@Test public void removeTextSpaningObjects()
	{
//		VersionedDocument doc = new VersionedDocument();
//		
//		doc.addText(0, "foobaz");
//		doc.addText(3, "bar");
//		
//		Collection<TextPart> parts = doc.getTextParts();
//		
//		assertEquals(1, doc.getTextParts().size());
//		
//		String actual = "";
//		for (TextPart p : parts)
//		{
//			actual += p.getText();
//		}
//		
//		assertEquals("", actual);
//		
//		assertEquals(0, 1);
	}
	
	@Test public void testSplitObject() 
	{
	}
	@Test public void testCreateChoice() 
	{
		VersionedDocument doc = new VersionedDocument();
		
		doc.addText(0, "foobar");
		doc.createChoice(0, 0, "t");
		
		Collection<TextPart> parts = doc.getTextParts();
		
		assertEquals(1, doc.getTextParts().size());
		
		String actual = "";
		for (TextPart p : parts)
		{
			actual += p.getText();
			assertEquals(p.getLabel(), new Label("t"));
		}
		
		assertEquals(1, doc.getSelectedTags().size());
		assertEquals("foobar", actual);
	}
	@Test public void testCreateChoiceWithSplit() {}
	@Test public void testRemoveChoice() {}
	
	@Test public void testAddAlternativeToChioce() {}
	@Test public void testAddAlternativeToVersionedObject() {}
	@Test public void testAddAlternativeToChoice() {}
	@Test public void testAddAlternativeWithExistingTag() {}
	@Test public void testAddAlternativeToInvalidLocatino() {}
	
	@Test public void testRemoveAlternative() {}
	@Test public void testRemoveAlternativeThatDoesntExist() {}
	@Test public void testRemoveAlternativeNotFromChoice() {}
	
	@Test public void testCreateDimension() 
	{
		VersionedDocument doc = new VersionedDocument();
		//doc.createDimension()
		
	}
	
	@Test public void testRemoveDimension() {}
	
	@Test public void testAddNewTagToDimension() {}
	@Test public void testAddExistingTagToDimension() {}
	@Test public void testAddTagFromOtherDimensionToDimension() {}
	@Test public void testAddEmptyTagToDimension() {}
	
	@Test public void testRemoveTagFromDimension() {}
	@Test public void testRemoveTagNotInDimensionFromDimension() {}
	
	@Test public void testRemoveTagFromDocument() {}
	
	@Test public void testCreateNewDimension() {}
	@Test public void testCreateNewDimensionName() {}
	
	@Test public void testChangeDimensionName() {}
	@Test public void testChangeDimensionNameToEmpty() {}
	
	
}
