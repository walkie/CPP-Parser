package editor.test;

import editor.Choice;
import editor.AbstractVersionedObject;
import editor.Label;
import editor.Let;
import editor.Variable;
import editor.VersionedObject;
import editor.util.*;

import static org.junit.Assert.*;

import org.junit.Test;

public class VersionedObjectTest {

	@Test
	public void testVersionedObjectTags() {
		VersionedObject o = new VersionedObject("foo");
		
		assertNotNull(o.tags());
	}

	@Test
	public void testVersionedObjectSelect() {
		AbstractVersionedObject o1 = new VersionedObject("foo");
		AbstractVersionedObject o2 = new VersionedObject("foo");
		o1 = o1.select("foo");

		assertEquals(o2.getText(), o1.getText());
	}
	@Test
	public void testChoiceSelect()
	{
		Label l1 = new Label(new String[] { "x" });
		Label l2 = new Label(new String[] { "y" });
		Choice c = new Choice();
		c.addAlternative(l1, new VersionedObject("xxx"));
		c.addAlternative(l2, new VersionedObject("yyy"));
		Tree<String> expected = new Tree<String>("xxx", false);
		Tree<String> actual = c.select("x").getText();
	
		assertEquals(expected, actual);
	}
	@Test
	public void testLetSelect()
	{
		Variable x = new Variable("x");
		AbstractVersionedObject bound = new VersionedObject("a");
		AbstractVersionedObject scope = new VersionedObject("b");
		Let l = new Let(x,bound,scope);
		AbstractVersionedObject selected = l.select("z");
		
		assertEquals(l, selected);
	}
	@Test
	public void testVariableSelect()
	{
		AbstractVersionedObject v1 = new Variable("x");
		AbstractVersionedObject v2 = v1.select("y");
		
		assertEquals(v1, v2);
	}
	
	@Test
	public void testChoiceGetText()
	{
		Choice c = new Choice();
		c.addAlternative(new Label(new String[] {"x"}), new VersionedObject("a"));
		c.addAlternative(new Label(new String[] {"y"}), new VersionedObject("b"));
		Tree<String> expected = new Tree<String>("", false);
		expected.addChild(new Tree<String>("a", true));
		expected.addChild(new Tree<String>("b", true));
		Tree<String> actual = c.getText();
		assertEquals(expected, actual);
	}
	@Test
	public void testLetGetText()
	{
		VersionedObject v = new VersionedObject("x");
		Variable v1 = new Variable("y");
		Variable v2 = new Variable("y");
		Let l = new Let(v1, v, v2);
		Tree<String> expected = new Tree<String>("x", true);
		Tree<String> actual = l.getText();
		assertEquals(expected,actual);
	}
	@Test
	public void testVariableGetText()
	{
		Variable v = new Variable("x");
		Tree<String> expected = new Tree<String>("", true);
		Tree<String> actual = v.getText();
		assertEquals(expected, actual);
	}
	@Test
	public void testVersionedObjectGetText()
	{
		VersionedObject v = new VersionedObject("x");
		Tree<String> expected = new Tree<String>("x", true);
		Tree<String> actual = v.getText();
		assertEquals(expected, actual);
	}
}
