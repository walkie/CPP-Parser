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
	Adapter adapter;
	String name;
	ArrayList<String> tags;
	ButtonGroup g = new ButtonGroup();
	
	public DimensionControl(Adapter adapter, String name, ArrayList<String> tags)
	{
		this.adapter = adapter;
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
		r.addActionListener(adapter.getSelectTagListener(name, tag));
		g.add(r);
		add(r);
		r.setSelected(true);
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
