package editor.ui.backup;

import java.awt.Color;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.util.ArrayList;
import java.util.Set;
import java.util.TreeSet;

import javax.swing.BoxLayout;
import javax.swing.ButtonGroup;
import javax.swing.JLabel;
import javax.swing.JMenuItem;
import javax.swing.JPanel;
import javax.swing.JPopupMenu;
import javax.swing.JRadioButton;
import javax.swing.JTextField;

import editor.model.Dim;
//import editor.model.Dimensions;
import editor.ui.dialogs.CreateTagDialog;
import editor.ui.dialogs.RemoveDimensionDialog;
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
		
		public RBLChecked(DocumentAdapter da, String tag)
		{
			this.da = da;
			this.tag = tag;
		}
		
		public void actionPerformed(ActionEvent e) 
		{
			da.select(tag);
		}
	}

	private class DimensionNameChange implements MouseListener, KeyListener
	{
		final Dim dim;
		final JLabel lblName;
		final JTextField txtName;
		
		public DimensionNameChange(Dim dim, JLabel lblName, JTextField txtName)
		{
			this.dim = dim;
			this.lblName = lblName;
			this.txtName = txtName;

			lblName.setVisible(true);
			txtName.setVisible(false);

			lblName.setText(dim.getName());
			txtName.setText(dim.getName());

			lblName.addMouseListener(this);
			txtName.addKeyListener(this);
		}

		private void flip()
		{
			if (lblName.isVisible() == true)
			{
				// show textbox
				lblName.setVisible(false);
				txtName.setVisible(true);
				txtName.requestFocus();
			}
			else
			{
				// show label and save name
				lblName.setVisible(true);
				txtName.setVisible(false);
				
//				dim.setName(txtName.getText());
				lblName.setText(txtName.getText());
			}
		}
		
		@Override public void mouseClicked(MouseEvent e)
		{
			if (e.getButton() == MouseEvent.BUTTON1)
			{
				if (e.getClickCount() == 2)
				{
					flip();
				}
			}
		}

		@Override public void keyPressed(KeyEvent e)
		{
			if (e.getKeyCode() == KeyEvent.VK_ENTER)
			{
				flip();
			}
			else if (e.getKeyCode() == KeyEvent.VK_ESCAPE)
			{
				lblName.setVisible(true);
				txtName.setVisible(false);
				txtName.setText(dim.getName());
			}
		}

		@Override public void mouseEntered(MouseEvent arg0) { }
		@Override public void mouseExited(MouseEvent arg0) { }
		@Override public void mousePressed(MouseEvent arg0) { }
		@Override public void mouseReleased(MouseEvent arg0) { }
		@Override public void keyReleased(KeyEvent arg0) { }
		@Override public void keyTyped(KeyEvent arg0) { }
	}
	
	public DimensionSelector(DocumentAdapter da, ColorManager colorManager)
	{
		this.da = da;
		this.colorManager = colorManager;
		
		setMaximumSize(new java.awt.Dimension(100,Integer.MAX_VALUE));
	}
	
	public void setDimensions(ArrayList<Dim> dimensions, Set<String> selectedTags)
	{
//		colorManager.setDimensions(dimensions);
		if (panel != null)
			panel.setVisible(false);
		panel = new JPanel();
		BoxLayout layout = new BoxLayout(panel, BoxLayout.Y_AXIS);
		panel.setLayout(layout);

		for (Dim d : dimensions)
		{
			JPanel p = new JPanel();
			p.setComponentPopupMenu(getDimensionPopupMenu(dimensions, d, null));
//			p.setBackground(colorManager.getColor(d.getTags()));
			ButtonGroup g = new ButtonGroup();
			JLabel lblName = new JLabel();
			JTextField txtName = new JTextField();
			new DimensionNameChange(d,lblName,txtName);
			p.add(lblName);
			p.add(txtName);
//			for (String t : d.getTags())
//			{
//				JRadioButton r = new JRadioButton(t);
//				r.setBackground(new Color(0, 0, 0, 0));
//				TreeSet<String> d2 = new TreeSet<String>();
//				d2.addAll(d.getTags());
//				d2.remove(t);
//				r.addActionListener(new RBLChecked(da, t));
//				r.setSelected(selectedTags.contains(t));
//				r.setComponentPopupMenu(getDimensionPopupMenu(dimensions, d, t));
//				g.add(r);
//				p.add(r);
//			}
			panel.add(p);
		}
		add(panel);
	}
	
	private JPopupMenu getDimensionPopupMenu(ArrayList<Dim> dimensions, Dim dim, String tag)
	{
		JMenuItem mi;
		JPopupMenu popup = new JPopupMenu();

		mi = new JMenuItem("Add Tag");
		mi.addActionListener(new TagAdder(dim));
		popup.add(mi);

		if (tag != null)
		{
			mi = new JMenuItem("Remove " + tag);
			mi.addActionListener(new TagRemover(tag, dim));
			popup.add(mi);
		}
		
		mi = new JMenuItem("Remove Dimension");
		mi.addActionListener(new DimensionRemover(dim));
		popup.add(mi);

		return popup;
	}

	class TagAdder implements ActionListener
	{
		Dim dim;
		
		public TagAdder(Dim dim)
		{
			this.dim = dim;
		}
		
		public void actionPerformed(ActionEvent e)
		{
//			new CreateTagDialog(dim, da);
		}
	}
	
	class TagRemover implements ActionListener
	{
		String tag;
		Dim dim;
		
		public TagRemover(String tag, Dim dim)
		{
			this.tag = tag;
			this.dim = dim;
		}
		
		public void actionPerformed(ActionEvent e)
		{
//			new RemoveTagDialog(tag, dim, da);
		}
	}

	class DimensionRemover implements ActionListener
	{
		Dim dim;
		
		public DimensionRemover(Dim dim)
		{
			this.dim = dim;
		}
		
		public void actionPerformed(ActionEvent e)
		{
			new RemoveDimensionDialog(dim, da);
		}
	}
}
