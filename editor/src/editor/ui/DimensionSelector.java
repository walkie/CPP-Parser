package editor.ui;

import java.awt.Color;
import java.awt.Dimension;
import java.util.ArrayList;
import java.util.Hashtable;

import javax.swing.BoxLayout;
import javax.swing.JPanel;

import editor.util.Debug;

public class DimensionSelector extends JPanel
{
	final Hashtable<String,DimensionControl> dims = new Hashtable<String,DimensionControl>();
	
	public DimensionSelector()
	{
		Debug.print("DimensionSelector() visible=" + isVisible());
		
		BoxLayout layout = new BoxLayout(this, BoxLayout.Y_AXIS);
		setLayout(layout);
		
		setBackground(new Color(200,200,240));
	}
	
	public void addDimension(String name, ArrayList<String> tags)
	{
		DimensionControl d = new DimensionControl(name, tags);
		dims.put(name, d);
		add(d);
		updateUI();
	}

	public String getSelectedDim()
	{
		return null;
	}
}
