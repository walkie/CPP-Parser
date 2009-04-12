package editor.test;

import editor.Choice;
import editor.Label;
import editor.Let;
import editor.Variable;
import editor.VersionedObject;

import static org.junit.Assert.*;

import org.junit.Test;

public class VersionedObjectTest {

	@Test
	public void testVersionedObjectTags() {
		VersionedObject o = new VersionedObject("foo");
		
		assertNotNull(o.tags());
	}
	
	@Test
	public void testChoiceGetText()
	{
		Choice c = new Choice();
		c.addAlternative(new Label(new String[] {"x"}), new VersionedObject("a"));
		c.addAlternative(new Label(new String[] {"y"}), new VersionedObject("b"));
		String expected = "ab";
		String actual = c.getText();
		assertEquals(expected, actual);
	}
	@Test
	public void testLetGetText()
	{
		VersionedObject v = new VersionedObject("x");
		Variable v1 = new Variable("y");
		Variable v2 = new Variable("y");
		Let l = new Let(v1, v, v2);
		String expected = "x";
		String actual = l.getText();
		assertEquals(expected,actual);
	}
	@Test
	public void testVariableGetText()
	{
		Variable v = new Variable("x");
		String expected = "";
		String actual = v.getText();
		assertEquals(expected, actual);
	}
	@Test
	public void testVersionedObjectGetText()
	{
		VersionedObject v = new VersionedObject("x");
		String expected = "x";
		String actual = v.getText();
		assertEquals(expected, actual);
	}
}
