package editor.ui;

import java.awt.Font;
import java.awt.GridLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.Collection;
import java.util.Set;
import java.util.TreeSet;

import javax.swing.BoxLayout;
import javax.swing.ButtonGroup;
import javax.swing.JEditorPane;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JMenu;
import javax.swing.JMenuBar;
import javax.swing.JMenuItem;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JRadioButton;
import javax.swing.JScrollPane;

import editor.AbstractVersionedObject;
import editor.ui.dialogs.AddAlternativeDialog;
import editor.ui.dialogs.AddTextDialog;
import editor.ui.dialogs.CreateChoiceDialog;
import editor.ui.dialogs.RemoveAlternativeDialog;
import editor.ui.dialogs.RemoveChoiceDialog;

public class Editor extends JFrame {
	private static final long serialVersionUID = 1L;
	private final JEditorPane e1 = new JEditorPane();
	private final JEditorPane e2 = new JEditorPane();
	private JPanel dimPanel = null;
	private DocumentAdapter da;
	
	public Editor(DocumentAdapter da)
	{
		this.da = da;
	
		setMenus();
	}

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
	
	public void setDimesionList(Collection<Set<String>> dimensions) {
		dimPanel = new JPanel();
		BoxLayout layout = new BoxLayout(dimPanel, BoxLayout.Y_AXIS);
		dimPanel.setLayout(layout);
		
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
				g.add(r);
				p.add(r);
			}
			dimPanel.add(p);
		}
	}

	public void setBottomText(String structuredText) {
		e2.setText(structuredText);
	}

	public void setTopDoc(AbstractVersionedObject doc) {
		da.setDocument(doc, e1, e2, this);
		da.setText();
		e1.addMouseListener(da);
	}

	public void showit()
	{
		BoxLayout layout  = new BoxLayout(getContentPane(), BoxLayout.X_AXIS);
		getContentPane().setLayout(layout);
		
		JPanel inner = new JPanel();
		inner.setLayout(new GridLayout(2,1));
		
		e1.setFont(new Font("Monospaced", 12, 12));
		e2.setFont(new Font("Monospaced", 12, 12));
		
		JScrollPane sp1 = new JScrollPane(e1);
		JScrollPane sp2 = new JScrollPane(e2);

		inner.add(sp1);
		inner.add(sp2);
		
		add(dimPanel);
		add(inner);
		
		setSize(new java.awt.Dimension(700,550));
		setVisible(true);
	}
	
	private void setMenus()
	{
		JMenuBar mb = new JMenuBar();
				
		mb.add(fileMenu());
		mb.add(editMenu());
		
		this.setJMenuBar(mb);
	}
	
	private JMenu fileMenu()
	{
		JMenu m = new JMenu("File");
		JMenuItem mi;
		
		mi = new JMenuItem("New");
		mi.addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(ActionEvent arg0) {
				da.newDoc();
			}
		});
		m.add(mi);

		mi = new JMenuItem("Open");
		mi.addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(ActionEvent e) {
				JOptionPane.showMessageDialog(null, "not implemented yet");
			}
		});
		m.add(mi);

		mi = new JMenuItem("Save");
		mi.addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(ActionEvent e) {
				JOptionPane.showMessageDialog(null, "not implemented yet");
			}
		});
		m.add(mi);

		mi = new JMenuItem("Exit");
		mi.addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(ActionEvent e) {
				setVisible(false);
			}
		});
		m.add(mi);
		
		return m;
	}

	private JMenu editMenu()
	{
		JMenu m = new JMenu("Edit");
		JMenuItem mi;
		
		mi = new JMenuItem("Add Text");
		mi.addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(ActionEvent arg0) {
				new AddTextDialog(da);
			}
		});
		m.add(mi);

		mi = new JMenuItem("Create Choice");
		mi.addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(ActionEvent e) {
				new CreateChoiceDialog(da);
			}
		});
		m.add(mi);

		mi = new JMenuItem("Remove Choice");
		mi.addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(ActionEvent e) {
				new RemoveChoiceDialog(da);
			}
		});
		m.add(mi);

		mi = new JMenuItem("Add Alternative");
		mi.addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(ActionEvent e) {
				new AddAlternativeDialog();
			}
		});
		m.add(mi);

		mi = new JMenuItem("Remove Alternative");
		mi.addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(ActionEvent e) {
				new RemoveAlternativeDialog();
			}
		});
		m.add(mi);
		
		return m;
	}
}
