package editor;

public class VersionedDocumentBuilder {

	private AbstractVersionedObject doc = null;
	
	public VersionedDocumentBuilder()
	{
		
	}
	
	public void addObject(AbstractVersionedObject d)
	{
		doc = d;
	}
	
	public AbstractVersionedObject getVersionedDocument()
	{
		return doc;
	}
}
