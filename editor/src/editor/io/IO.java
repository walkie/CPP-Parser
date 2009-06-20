package editor.io;

import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;

import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBException;
import javax.xml.bind.Marshaller;
import javax.xml.bind.Unmarshaller;

public class IO
{
	public void read(String fileName)
	{
		try 
		{
			JAXBContext jc = JAXBContext.newInstance(Document.class);
			Unmarshaller u = jc.createUnmarshaller();
			FileInputStream fs = new FileInputStream(fileName);
			java.lang.Object doc = u.unmarshal(fs);
			editor.io.Document d = (editor.io.Document) doc;
			Dimensions dims = d.getDimensions();
			for (int i=0;i<dims.getDimension().size();i++)
			{
				Dimension dim = dims.getDimension().get(i);
				System.out.printf("Name: %s Default %s", dim.getName(), dim.getDefault());
			}
		} 
		catch (FileNotFoundException e)
		{
			e.printStackTrace();
		}
		catch (JAXBException e)
		{
			e.printStackTrace();
		}
	}

	public void write(String fileName)
	{
		try 
		{
			JAXBContext jc = JAXBContext.newInstance(Document.class);
			Document doc = new Document();
			Dimensions dims = new Dimensions();
			Dimension dim = new Dimension();
			dim.setName("Java Version");
			Tags tags = new Tags();
			tags.getTag().add("1.4");
			tags.getTag().add("5.0");
			dim.getTags().add(tags);
			dims.getDimension().add(dim);
			doc.setDimensions(dims);
			Marshaller m = jc.createMarshaller();
			m.setProperty(Marshaller.JAXB_FORMATTED_OUTPUT, Boolean.TRUE);
			m.marshal(doc, new FileOutputStream(fileName));
		} 
		catch (JAXBException e)
		{
			e.printStackTrace();
		}
		catch (FileNotFoundException e)
		{
			e.printStackTrace();
		}
	}
}
