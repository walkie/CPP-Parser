package editor.ui;

import java.awt.Color;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.Set;
import java.util.TreeSet;

import javax.swing.BoxLayout;
import javax.swing.ButtonGroup;
import javax.swing.JMenuItem;
import javax.swing.JPanel;
import javax.swing.JPopupMenu;
import javax.swing.JRadioButton;

import editor.Dimension;
import editor.Dimensions;
import editor.ui.dialogs.CreateTagDialog;
import editor.ui.dialogs.RemoveTagDialog;

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
			JPanel p = new JPanel();
			p.setComponentPopupMenu(getDimensionPopupMenu(d, null));
			p.setBackground(colorManager.getColor(d.tags()));
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
				r.setComponentPopupMenu(getDimensionPopupMenu(d, t));
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
		
		public TagAdder(Dimension dim)
		{
			this.dim = dim;
		}
		
		public void actionPerformed(ActionEvent e)
		{
			new CreateTagDialog(dim, da);
		}
	}
	
	class TagRemover implements ActionListener
	{
		String tag;
		Dimension dim;
		
		public TagRemover(String tag, Dimension dim)
		{
			this.tag = tag;
			this.dim = dim;
		}
		
		public void actionPerformed(ActionEvent e)
		{
			new RemoveTagDialog(tag, dim, da);
		}
	}
	
	private JPopupMenu getDimensionPopupMenu(Dimension d, String tag)
	{
		JMenuItem mi;
		JPopupMenu popup = new JPopupMenu();

		mi = new JMenuItem("Add Tag");
		mi.addActionListener(new TagAdder(d));
		popup.add(mi);

		if (tag != null)
		{
			mi = new JMenuItem("Remove " + tag);
			mi.addActionListener(new TagRemover(tag, d));
			popup.add(mi);
		}
		
		return popup;
	}
}
