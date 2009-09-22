package editor.ui;

import java.awt.Color;
import java.awt.Dimension;
import java.util.ArrayList;
import java.util.Hashtable;

import javax.swing.BoxLayout;
import javax.swing.JLabel;
import javax.swing.JPanel;

import editor.model.Dim;
import editor.util.Debug;

public class DimensionSelector extends JPanel
{
	final Hashtable<String,DimensionControl> dims = new Hashtable<String,DimensionControl>();
	Adapter adapter;
	
	public DimensionSelector()
	{
		Debug.print("DimensionSelector() visible=" + isVisible());
		
		BoxLayout layout = new BoxLayout(this, BoxLayout.Y_AXIS);
		setLayout(layout);
		
		JPanel title = new JPanel();
		title.add(new JLabel("Dimensions"));
		title.setPreferredSize(new Dimension(200,20));
		add(title);
		
		setBackground(new Color(200,200,240));
	}
	
	public void addDimension(String name, ArrayList<String> tags)
	{
		DimensionControl d = new DimensionControl(adapter, name, tags);
		dims.put(name, d);
		add(d);
		updateUI();
	}

	public String getSelectedDim()
	{
		return null;
	}

	public void updateDim(Dim dim)
	{
		DimensionControl dc = dims.get(dim.getName());
		
		dc.setTags(dim.getTags());
		updateUI();
	}

	public void setAdapter(Adapter adapter)
	{
		this.adapter = adapter;
	}
}
