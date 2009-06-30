package editor.io;

import java.util.List;

import javax.xml.bind.JAXBElement;

import editor.io.Part.SubObjects;

public class Convert
{
	public static editor.io.Document fromModel(editor.model.Document document)
	{
		editor.io.Document doc = new editor.io.Document();
		doc.setDimensions(fromModelDimensions(document.getDimensions()));

		doc.getObject().add(fromModelObject(document.getObject()));
		
		return doc;
	}

	private static editor.io.Dimensions fromModelDimensions(editor.model.Dimensions mDimensions)
	{
		editor.io.Dimensions dimensions = new editor.io.Dimensions();

		for (editor.model.Dimension mDim : mDimensions)
		{
			editor.io.Dimension dim = new editor.io.Dimension();
			dim.setName(mDim.getName());
			Tags tags = new Tags();
			for (String tag : mDim.getTags())
			{
				tags.getTag().add(tag);
			}
			dim.getTags().add(tags);
			dimensions.getDimension().add(dim);
		}
		return dimensions;
	}

	private static JAXBElement<?> fromModelObject(editor.model.AbstractVersionedObject mObject)
	{
		if (mObject instanceof editor.model.VersionedObject)
			return fromModelVersionedObject((editor.model.VersionedObject)mObject);
		if (mObject instanceof editor.model.Choice)
			return fromModelChoice((editor.model.Choice)mObject);
		return null;
	}

	private static JAXBElement<?> fromModelVersionedObject(editor.model.VersionedObject mObject)
	{
		editor.io.Part part = new editor.io.Part();
		part.setData(mObject.getValue());
		
		Part.SubObjects subObjects = new Part.SubObjects();
		for (editor.model.AbstractVersionedObject v : mObject.getSubObjects())
		{
			subObjects.getObject().add(fromModelObject(v));
		}
		part.setSubObjects(subObjects);
		
		ObjectFactory fact = new ObjectFactory();
		return fact.createPart(part);
	}

	private static JAXBElement<?> fromModelChoice(editor.model.Choice mChoice)
	{
		editor.io.Choice choice = new editor.io.Choice();
		choice.setAlternatives(new Choice.Alternatives());
		for (String tag : mChoice.getTags())
		{
			Alternative alternative = new Alternative();
			Tags tags = new Tags();
			tags.getTag().add(tag);
			alternative.setLabel(tags);
			alternative.setObject(fromModelObject(mChoice.getAlternative(tag)));
			choice.getAlternatives().getAlternative().add(alternative);
		}
		
		ObjectFactory fact = new ObjectFactory();
		return fact.createChoice(choice);
	}
	
	public static editor.model.Document toModel(editor.io.Document document)
	{
		editor.model.Document mDocument = new editor.model.Document();
	
		for (editor.io.Dimension dimension : document.getDimensions().getDimension())
		{
			mDocument.addDimension(toModelDimension(dimension));
		}
		
		if (document.getObject().size() == 1)
		{
			List<JAXBElement<?>> object = document.getObject();
			JAXBElement<?> e = object.get(0);
			java.lang.Object o = (java.lang.Object)e.getValue();
			mDocument.setObject(toModelObject(mDocument, o));
		}
		else
		{
			mDocument.setObject(new editor.model.VersionedObject(mDocument, ""));		
		}

		return mDocument;
	}

	private static editor.model.Dimension toModelDimension(editor.io.Dimension dimension)
	{
		editor.model.Dimension mDimension = new editor.model.Dimension();
		mDimension.setName(dimension.getName());
		for (Tags tags : dimension.getTags())
		{
			for (String tag : tags.getTag())
			{
				mDimension.addTag(tag);
			}
		}
		
		return mDimension;
	}

	private static editor.model.AbstractVersionedObject toModelObject(editor.model.Document mDocument, java.lang.Object object)
	{
		if (object instanceof editor.io.Part)
			return toModelFromPart(mDocument, (editor.io.Part)object);
		if (object instanceof editor.io.Choice)
			return toModelFromChoice(mDocument, (editor.io.Choice)object);
			
		return new editor.model.VersionedObject(mDocument, "");
	}

	private static editor.model.AbstractVersionedObject toModelFromPart(editor.model.Document mDocument, editor.io.Part part)
	{
		editor.model.VersionedObject v = new editor.model.VersionedObject(mDocument, part.getData().toString());
		
		SubObjects subObjects = part.getSubObjects();
		for (JAXBElement<?> elem : subObjects.getObject())
		{
			v.addSubObject(toModelObject(mDocument, elem.getValue()));
		}
		
		return v;
	}

	private static editor.model.AbstractVersionedObject toModelFromChoice(editor.model.Document mDocument, editor.io.Choice choice)
	{
		editor.model.Dimension dimension = new editor.model.Dimension();
		editor.model.Choice mChoice = new editor.model.Choice(dimension);
		
		for (Alternative alternative : choice.getAlternatives().getAlternative())
		{
			for (String tag : alternative.getLabel().getTag())
			{
				mChoice.addAlternative(tag, toModelObject(mDocument, alternative.getObject().getValue()));
			}
		}

		return mChoice;
	}
}
