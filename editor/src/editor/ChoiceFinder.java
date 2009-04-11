package editor;

import java.util.ArrayList;
import java.util.Collection;

public class ChoiceFinder extends VersionedObjectVisitor {

	private ArrayList<Choice> choices = new ArrayList<Choice>();
	
	public Collection<Choice> getChoices()
	{
		return choices;
	}

	@Override
	public void visit(Choice choice) {
		choices.add(choice);
		super.visit(choice);
	}
}
