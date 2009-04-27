package editor.ui;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.Collection;
import java.util.Set;
import java.util.TreeSet;

import javax.swing.BoxLayout;
import javax.swing.ButtonGroup;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JRadioButton;

public class DimensionSelector extends JPanel 
{
	private static final long serialVersionUID = 1L;
	private DocumentAdapter da;
	private JPanel panel = null;
	
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
		
		@Override
		public void actionPerformed(ActionEvent e) 
		{
			da.unselect(dim);
			da.select(tag);
		}
	}

	public DimensionSelector(DocumentAdapter da)
	{
		this.da = da;

	}
	
	public void setDimensions(Collection<Set<String>> dimensions, Collection<String> selectedTags)
	{
		if (panel != null)
			panel.setVisible(false);
		panel = new JPanel();
		BoxLayout layout = new BoxLayout(panel, BoxLayout.Y_AXIS);
		panel.setLayout(layout);

		for (Set<String> d : dimensions)
		{
			JPanel p = new JPanel();
			p.add(new JLabel("Dimension"));
			ButtonGroup g = new ButtonGroup();
			for (String t : d)
			{
				JRadioButton r = new JRadioButton(t);
				TreeSet<String> d2 = new TreeSet<String>();
				d2.addAll(d);
				d2.remove(t);
				r.addActionListener(new RBLChecked(da, t, d2));
				r.setSelected(selectedTags.contains(t));
				g.add(r);
				p.add(r);
			}
			panel.add(p);
		}
		add(panel);
	}
}
