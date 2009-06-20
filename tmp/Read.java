import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBException;
import javax.xml.bind.Marshaller;
import tosc.*;
public class Read {

	/**
	 * @param args
	 */
	public static void main(String[] args) {
		try {
			JAXBContext jc = JAXBContext.newInstance(Document.class);
			/*Unmarshaller u = jc.createUnmarshaller();
			FileInputStream fs = new FileInputStream("C:\\Users\\Weinian He\\workspace\\ToscIO\\src\\Schema4Test.xml");
			java.lang.Object doc = u.unmarshal(fs);
			tosc.Document d = (Document) doc;
			Dimensions dimn = ((Document)doc).getDimensions();
			for (int i=0;i<dimn.getDimension().size();i++)
			{
				Dimension dim = dimn.getDimension().get(i);
				System.out.printf("Name: %s Default %s", dim.getName(), dim.getDefault());
			}*/
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
			m.marshal(doc, System.out);
		} catch (JAXBException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}

	}

}
