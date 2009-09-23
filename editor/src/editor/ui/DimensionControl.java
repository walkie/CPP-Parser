package editor.ui;

import java.awt.Dimension;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.util.ArrayList;

import javax.swing.ButtonGroup;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JRadioButton;
import javax.swing.JTextField;

import editor.util.Debug;

public class DimensionControl extends JPanel
{
	Adapter adapter;
	String name;
	ArrayList<String> tags;
	ButtonGroup g = new ButtonGroup();
	JRadioButton dimRadioButton;
	DimensionSelector ds;
	
	public DimensionControl(Adapter adapter, DimensionSelector ds, String name, ArrayList<String> tags)
	{
		this.adapter = adapter;
		this.ds = ds;
		this.name = name;
		this.tags = new ArrayList<String>();
		this.tags.addAll(tags);
		setUI();
		setPreferredSize(new Dimension(200, 40));
	}

	private void setUI()
	{
		addDimName();
		
		for (String tag : tags)
		{
			addTag(tag);
		}
	}
	
	private void addDimName()
	{
		final JLabel l = new JLabel(name);
		final JTextField t = new JTextField();
		dimRadioButton = new JRadioButton();
		adapter.getDimensionButtonGroup().add(dimRadioButton);
		dimRadioButton.setSelected(true);
		dimRadioButton.addActionListener(adapter.dimensionSelectionListener(name));
		l.addMouseListener(new MouseListener() {
			public void mouseClicked(MouseEvent e)
			{
				if (e.getClickCount() == 2)
				{
					l.setVisible(false);
					t.setText(l.getText());
					t.setVisible(true);
				}
			}
			public void mouseEntered(MouseEvent e) { }
			public void mouseExited(MouseEvent e) { }
			public void mousePressed(MouseEvent e) { }
			public void mouseReleased(MouseEvent e) { }
		});
		t.addKeyListener(new KeyListener() {
			public void keyPressed(KeyEvent e) { }
			public void keyReleased(KeyEvent e) { }
			public void keyTyped(KeyEvent e) 
			{
				if (e.getKeyChar() == '\n')
				{
					if (t.getText() != "")
					{
						adapter.changeDimName(l.getText(), t.getText());
						if (l.getText().equals(ds.getSelectedDim()))
							ds.setSelectedDim(t.getText());
						t.setVisible(false);
						l.setText(t.getText());
						l.setVisible(true);
					}
				}
			}
		});
		add(dimRadioButton);
		add(t);
		add(l);
		t.setVisible(false);
	}
	
	private void addTag(String tag)
	{
		final JLabel l = new JLabel(tag);
		final JTextField t = new JTextField(tag);
		final JRadioButton r = new JRadioButton();
		r.addActionListener(adapter.getSelectTagListener(name, l));
		l.addMouseListener(new MouseListener() {
			public void mouseClicked(MouseEvent e)
			{
				if (e.getClickCount() == 2)
				{
					l.setVisible(false);
					t.setText(l.getText());
					t.setVisible(true);
				}
			}
			public void mouseEntered(MouseEvent e) { }
			public void mouseExited(MouseEvent e) { }
			public void mousePressed(MouseEvent e) { }
			public void mouseReleased(MouseEvent e) { }
		});
		t.addKeyListener(new KeyListener() {
			public void keyPressed(KeyEvent e) { }
			public void keyReleased(KeyEvent e) { }
			public void keyTyped(KeyEvent e) 
			{
				if (e.getKeyChar() == '\n')
				{
					if (t.getText() != "")
					{
						ds.
						adapter.changeTag(name, l.getText(), t.getText());
						t.setVisible(false);
						l.setText(t.getText());
						l.setVisible(true);
					}
				}
			}
		});
		g.add(r);
		add(r);
		add(t);
		add(l);
		r.setSelected(true);
		t.setVisible(false);
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
