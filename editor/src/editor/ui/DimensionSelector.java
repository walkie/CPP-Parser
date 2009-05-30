package editor.ui;

import java.awt.Color;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.Set;
import java.util.TreeSet;

import javax.swing.BoxLayout;
import javax.swing.ButtonGroup;
import javax.swing.JLabel;
import javax.swing.JMenuItem;
import javax.swing.JPanel;
import javax.swing.JPopupMenu;
import javax.swing.JRadioButton;

import editor.Dimension;
import editor.Dimensions;

public class DimensionSelector extends JPanel 
{
	private static final long serialVersionUID = 1L;
	private DocumentAdapter da;
	private JPanel panel = null;
	private final ColorManager colorManager;
	
	private class RBLChecked implements ActionListener
	{
		DocumentAdapter da;
		String tag;
		Set<String> dim;
		
		public RBLChecked(DocumentAdapter da, String tag, Set<String> dim)
		{
			this.da = da;
			this.tag = tag;
			this.dim = dim;
		}
		
		public void actionPerformed(ActionEvent e) 
		{
			da.select(tag);
		}
	}

	public DimensionSelector(DocumentAdapter da, ColorManager colorManager)
	{
		this.da = da;
		this.colorManager = colorManager;
		
		setMaximumSize(new java.awt.Dimension(100,Integer.MAX_VALUE));
	}
	
	public void setDimensions(Dimensions dimensions, Set<String> selectedTags)
	{
		colorManager.setDimensions(dimensions);
		if (panel != null)
			panel.setVisible(false);
		panel = new JPanel();
		BoxLayout layout = new BoxLayout(panel, BoxLayout.Y_AXIS);
		panel.setLayout(layout);

		for (Dimension d : dimensions)
		{
			JPopupMenu popup = getDimensionPopupMenu(d);
			JPanel p = new JPanel();
			p.setComponentPopupMenu(popup);
			p.setBackground(colorManager.getColor(d.tags()));
			p.add(new JLabel("Dimension"));
			ButtonGroup g = new ButtonGroup();
			for (String t : d.tags())
			{
				JRadioButton r = new JRadioButton(t);
				r.setBackground(new Color(0, 0, 0, 0));
				TreeSet<String> d2 = new TreeSet<String>();
				d2.addAll(d.tags());
				d2.remove(t);
				r.addActionListener(new RBLChecked(da, t, d2));
				r.setSelected(selectedTags.contains(t));
				r.setComponentPopupMenu(popup);
				g.add(r);
				p.add(r);
			}
			panel.add(p);
		}
		add(panel);
	}

	class TagAdder implements ActionListener
	{
		Dimension dim;
		
		public TagAdder(Dimension d)
		{
			this.dim = d;
		}
		
		public void actionPerformed(ActionEvent e)
		{
			dim.addTag("T");
		}
	}
	private JPopupMenu getDimensionPopupMenu(Dimension d)
	{
		JPopupMenu popup = new JPopupMenu();
		JMenuItem mi = new JMenuItem("Add Tag");
		mi.addActionListener(new TagAdder(d));
		popup.add(mi);
		return popup;
	}
}
