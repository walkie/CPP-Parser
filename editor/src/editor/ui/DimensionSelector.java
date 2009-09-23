package editor.ui;

import java.awt.Color;
import java.awt.Dimension;
import java.util.ArrayList;
import java.util.Hashtable;

import javax.swing.BoxLayout;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JRadioButton;

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
		
		setBackground(new Color(200,200,240));
	}
	
	protected JPanel newDimPanel()
	{
		JPanel panel = new JPanel();
		JRadioButton r = new JRadioButton();
		panel.add(r);
		panel.add(new JLabel("new dimension"));
		r.addActionListener(adapter.dimensionSelectionListener(null));
		adapter.getDimensionButtonGroup().add(r);
		r.setSelected(true);
		return panel;
	}
	
	public void addDimension(String name, ArrayList<String> tags)
	{
		if (!dims.containsKey(name))
		{
			DimensionControl d = new DimensionControl(adapter, this, name, tags);
			dims.put(name, d);
			add(d);
			selectedDim = name;
			updateUI();
		}
	}

	public void updateDim(Dim dim)
	{
		DimensionControl dc = dims.get(dim.getName());
		
		dc.setTags(dim.getTags());
		updateUI();
	}

	public void initUI(Adapter adapter)
	{
		this.adapter = adapter;
		
		JPanel title = new JPanel();
		title.add(new JLabel("Dimensions"));
		title.setPreferredSize(new Dimension(200,20));
		add(title);
		
		add(newDimPanel());
	}

	String selectedDim = null;
	public void setSelectedDim(String name)
	{
		selectedDim = name;
	}
	
	public String getSelectedDim()
	{
		return selectedDim;
	}
}
