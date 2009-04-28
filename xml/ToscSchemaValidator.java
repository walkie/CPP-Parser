import org.xml.sax.SAXException;
import org.xml.sax.SAXNotRecognizedException;
import org.xml.sax.SAXNotSupportedException;
import org.xml.sax.SAXParseException;
import org.xml.sax.helpers.DefaultHandler;

import com.sun.org.apache.xerces.internal.parsers.SAXParser;



public class ToscSchemaValidator extends DefaultHandler{
	public boolean validationError = false;
	public SAXParseException saxParseException = null;
	public void error(SAXParseException exception) throws SAXException
	{
		validationError = true;
		saxParseException = exception;
	}
	
	public void fatalError(SAXParseException exception) throws SAXException
	{
		validationError = true;
		saxParseException = exception;		
	}
	public void warning(SAXParseException exception) throws SAXException
	{
	}
	
	

	
}


// import java.io.File;
// import javax.xml.parsers.DocumentBuilder;
//import javax.xml.parsers.DocumentBuilderFactory;


/*public class ToscSchemaValidator {

	static final String JAXP_SCHEMA_LANGUAGE = "http://java.sun.com/xml/jaxp/properties/schemaLanguage";
	static final String W3C_XML_SCHEMA = "http://www.w3.org/2001/XMLSchema";
	
	
	private static String schemaName ;
	
	public static void Validate(String schema)
	{
		schemaName = schema;
		
		DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();
		factory.setNamespaceAware(true);
		factory.setValidating(true);
		try{
			DocumentBuilder builder = factory.newDocumentBuilder();
			builder.parse(new File("src\\Test2.xml"));
			factory.setAttribute(JAXP_SCHEMA_LANGUAGE, schemaName);
		}
		catch (IllegalArgumentException er)
		{
			System.out.println("error just happen");
		}
		catch (Exception e)
		{
			System.out.println("Just an error" + e.getMessage());
		}
		
		System.out.println("Schema Name" +schemaName);

	}
	
}
*/