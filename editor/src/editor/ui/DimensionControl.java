package editor.ui;

import java.awt.Dimension;
import java.util.ArrayList;

import javax.swing.ButtonGroup;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JRadioButton;

import editor.util.Debug;

public class DimensionControl extends JPanel
{
	String name;
	ArrayList<String> tags;
	ButtonGroup g = new ButtonGroup();
	
	public DimensionControl(String name, ArrayList<String> tags)
	{
		this.name = name;
		this.tags = new ArrayList<String>();
		this.tags.addAll(tags);
		setUI();
		setPreferredSize(new Dimension(200, 40));
	}

	private void setUI()
	{
		add(new JLabel(name));
			
		for (String tag : tags)
		{
			addTag(tag);
		}
	}

	private void addTag(String tag)
	{
		JRadioButton r = new JRadioButton(tag);
		g.add(r);
		add(r);
	}
	
	public void setTags(ArrayList<String> tags)
	{
		for (String tag : tags)
		{
			if (!this.tags.contains(tag))
				addTag(tag);
		}
		for (String tag : this.tags)
		{
			if (!tags.contains(tag))
				removeTag(tag);
		}
	}

	private void removeTag(String tag)
	{
		Debug.print("need to remove");
	}
}
