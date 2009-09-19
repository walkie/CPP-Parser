package editor.test;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

import java.io.FileNotFoundException;
import java.io.IOException;

import javax.xml.bind.JAXBException;

import org.junit.Before;
import org.junit.Test;

import editor.controller.VersionedDocument;
import editor.model.Document;

public class VersionedDocumentTests
{
//	VersionedDocument docWithJustText = null;

//	@Before public void setUp()
//	{
//		VersionedObject v = new VersionedObject(new Document(), "ToSC");
//		docWithJustText = new VersionedDocument(v);
//	}
//	
//	@Test public void testCreateChoice() 
//	{
//		System.out.println("testCreateChoice()");
//		
//		docWithJustText.createChoice(0, 0, "t");
//		
//		System.out.println(docWithJustText.getDocument().getText());
//		
//		System.out.println(docWithJustText.getDimensions().size());
//		assertEquals(1, docWithJustText.getDimensions().size());
//		System.out.println(docWithJustText.getDimensions().first().getTags());
//		assertTrue(docWithJustText.getDimensions().first().getTags().contains("t"));
//	}
//	
//	@Test public void testIOUnmarshall() throws FileNotFoundException, JAXBException
//	{
//		try {
//			String path = new java.io.File(".").getCanonicalPath();
//			System.out.println(path);
//		} catch (IOException e) {
//			// TODO Auto-generated catch block
//			e.printStackTrace();
//		}
//		editor.io.IO testIO = new editor.io.IO();
//		testIO.read("src/editor/test/TestIO.xml");
//	}
}
