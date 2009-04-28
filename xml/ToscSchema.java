import java.io.IOException;

import org.xml.sax.SAXException;
import org.xml.sax.SAXNotRecognizedException;
import org.xml.sax.SAXNotSupportedException;

import com.sun.org.apache.xerces.internal.parsers.SAXParser;


public class ToscSchema {

	/**
	 * @param args
	 */
	public static void main(String[] args) {
		String xml = "src\\Test1.xml";
		String schema = "src\\Schema.xsd";
		SAXParser parser = new SAXParser();
		ToscSchemaValidator handler = new ToscSchemaValidator();
		parser.setErrorHandler(handler);
		try {

			parser.setFeature("http://xml.org/sax/features/validation", true);
			parser.setFeature("http://apache.org/xml/features/validation/schema", true);
			parser.setFeature("http://apache.org/xml/features/validation/schema-full-checking",true); 
			parser.setProperty("http://apache.org/xml/properties/schema/external-noNamespaceSchemaLocation",schema);
			parser.parse(xml);
			if(handler.validationError==true)           
				System.out.println("XML Document has Error:"+handler.validationError +" Line: "+handler.saxParseException.getLineNumber()+""+handler.saxParseException.getMessage());
			else                   
			    System.out.println("XML Document is valid");  	
		} catch (SAXNotRecognizedException e) {
			System.out.println("I don't think so ");
			System.out.println("I don't think so ");
			e.printStackTrace();
		} catch (SAXNotSupportedException e) {
			System.out.println("I don't think so ");
			e.printStackTrace();
		} catch (SAXException e) {
			System.out.println("I don't think so ");
			e.printStackTrace();
		} catch (IOException e) {
			System.out.println("I don't think so ");
			e.printStackTrace();
		} catch (Exception e){
			System.out.println("I don't think so ");
			e.printStackTrace();
		}
		
		System.out.println("Success");

	}

}
