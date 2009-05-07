package editor.util;

import java.util.ArrayList;
import java.util.Collection;

import editor.Choice;
import editor.VersionedObject;

public class ChoiceFinder extends VersionedObjectVisitor {

	private int pos = -1;
	private ArrayList<Choice> choices = new ArrayList<Choice>();
	
	public ChoiceFinder()
	{
		this.pos = -1;
	}
	
	public ChoiceFinder(int pos)
	{
		this.pos = pos;
	}

	public Collection<Choice> getChoices()
	{
		return choices;
	}

	public Choice getChoice()
	{
		if (choices.size() > 0)
			return choices.get(0);
		else
			return null;
	}
	
	@Override
	public void visit(Choice choice)
	{
		if (pos <= 0)
		{
			choices.add(choice);
			super.visit(choice);
		}
	}
	
	public void visit(VersionedObject v)
	{
		pos -= v.getValue().length();
		super.visit(v);
	}
}
