package editor.model;

public class TreeTest
{
	public static void main(String[] args)
	{
		// {hello,goodbye} world
		Document doc = new Document();
		VersionedObject v = new VersionedObject(doc, "");
		VersionedObject v1 = new VersionedObject(doc, "hello");
		VersionedObject v2 = new VersionedObject(doc, "goodbye");
		VersionedObject v3 = new VersionedObject(doc, " ");
		VersionedObject v4 = new VersionedObject(doc, "world");
		VersionedObject v5 = new VersionedObject(doc, "planet");
		VersionedObject v6 = new VersionedObject(doc, "earth");
		VersionedObject v7 = new VersionedObject(doc, "adios");
		new Tree(v);
		Choice c = new Choice(new Dimension());
		v.addSubObject(c);
		c.addAlternative("h", v1);
		c.addAlternative("g", v2);
		v.addSubObject(v3);
		v.addSubObject(v4);
		
		System.out.println(v.getText());

		Dimension d1 = new Dimension();
		d1.setName("d1");
		d1.addTag("h");
		d1.addTag("g");
		Dimension d2 = new Dimension();
		d1.setName("d2");
		d1.addTag("w");
		d1.addTag("p");
		doc.addDimension(d1);
		doc.addDimension(d2);
		doc.setObj(v);
		// 
		Choice c2 = v4.createChoice("w");
		c2.addAlternative("p", v5);

		System.out.println(v.getText());

		v2.removeText(0,1);
		v2.removeText(5,1);
		
		System.out.println(v.getText());

		v2.addText(0, "G");
		v2.addText(6,"e");
		
		System.out.println(v.getText());		
		
		v5.removeChoice();
		
		System.out.println(v.getText());
		
		v5.createChoice("p", "e", v6);
		c.addAlternative("p", v7);
		
		System.out.println(v.getText());
		
		doc.removeTag("p");
		
		System.out.println(v.getText());
		
		doc.removeDimension(d1);
		
		System.out.println(v.getText());
		
	}
}