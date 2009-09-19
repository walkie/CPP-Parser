package editor.ui;

import java.awt.*;
import javax.swing.*;

public class Editor extends JFrame
{
	public Editor()
	{
		GridBagConstraints constraints = new java.awt.GridBagConstraints();
		
		constraints.gridheight = 1;
		constraints.gridwidth = 2;
		constraints.fill = GridBagConstraints.BOTH;
		
		java.awt.GridBagLayout layout = new GridBagLayout();
		layout.setConstraints(this, constraints);
		
		setLayout(layout);
		
		setSize(new Dimension(800,600));
	}
	
	public void addLeft(Component comp)
	{
		GridBagConstraints c = new GridBagConstraints();
		c.gridx = 0;
		c.gridy = 0;
		c.weightx = 0.2;
		c.weighty = 1.0;
		c.fill = GridBagConstraints.BOTH;
		
		add(comp, c);
	}
	
	public void addRight(Component comp)
	{
		GridBagConstraints c = new GridBagConstraints();
		c.gridx = 1;
		c.gridy = 0;
		c.weightx = 0.8;
		c.weighty = 1.0;
		c.fill = GridBagConstraints.BOTH;
		
		add(comp, c);
	}
}
