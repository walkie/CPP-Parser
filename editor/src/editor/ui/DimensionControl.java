package editor.ui;

import java.awt.Dimension;
import java.util.ArrayList;

import javax.swing.ButtonGroup;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JRadioButton;

public class DimensionControl extends JPanel
{
	String name;
	ArrayList<String> tags;
	
	public DimensionControl(String name, ArrayList<String> tags)
	{
		this.name = name;
		this.tags = tags;
		setUI();
		setPreferredSize(new Dimension(200, 40));
	}

	private void setUI()
	{
		add(new JLabel(name));
		
		ButtonGroup g = new ButtonGroup();
		for (String tag : tags)
		{
			JRadioButton r = new JRadioButton(tag);
			g.add(r);
			add(r);
		}
	}
}
