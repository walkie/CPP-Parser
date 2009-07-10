package editor.io;

import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;

import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBElement;
import javax.xml.bind.JAXBException;
import javax.xml.bind.Marshaller;
import javax.xml.bind.Unmarshaller;

public class IO
{
	public editor.model.Document read(String fileName) throws JAXBException, FileNotFoundException
	{
		JAXBContext jc = JAXBContext.newInstance(Document.class,Dimensions.class,Dimension.class,Tags.class);
		Unmarshaller u = jc.createUnmarshaller();
		FileInputStream fs = new FileInputStream(fileName);
		JAXBElement<?> doc = (JAXBElement<?>)u.unmarshal(fs);
		editor.io.Document d = (Document)doc.getValue();
		return Convert.toModel(d);
	}

	public void write(String fileName, editor.model.Document doc)
	{
		try 
		{
			Document d = Convert.fromModel(doc);
			JAXBContext jc = JAXBContext.newInstance(Document.class);
			Marshaller m = jc.createMarshaller();
			m.setProperty(Marshaller.JAXB_FORMATTED_OUTPUT, Boolean.TRUE);
			m.marshal(d, new FileOutputStream(fileName));
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
