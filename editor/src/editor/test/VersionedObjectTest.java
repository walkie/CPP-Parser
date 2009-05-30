package editor.test;

import editor.VersionedObject;

import static org.junit.Assert.*;

import org.junit.Test;

public class VersionedObjectTest {

	@Test
	public void testVersionedObjectTags() {
		VersionedObject o = new VersionedObject("foo");
		
		assertNotNull(o.tags());
	}
}
