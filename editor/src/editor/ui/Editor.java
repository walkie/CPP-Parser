package editor.ui;

import java.awt.Font;
import java.awt.GridLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.Collection;
import java.util.Set;
import java.util.TreeSet;

import javax.swing.BoxLayout;
import javax.swing.JComponent;
import javax.swing.JEditorPane;
import javax.swing.JFrame;
import javax.swing.JMenu;
import javax.swing.JMenuBar;
import javax.swing.JMenuItem;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JPopupMenu;
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
	private DimensionSelector dimensionSelector;
	private DocumentAdapter da;
	private ColorManager colorManager;
	
	public Editor(DocumentAdapter da)
	{
		this.da = da;
		this.colorManager = new ColorManager();
		this.dimensionSelector = new DimensionSelector(da, colorManager);
		
		setMenus();
	}
	
	public void setDimensionList(Collection<Set<String>> dimensions) 
	{
		dimensionSelector.setDimensions(dimensions, new TreeSet<String>());
	}

	public void setBottomText(String structuredText) {
		e2.setText(structuredText);
	}

	public void setTopDoc(AbstractVersionedObject doc) {
		da.setDocument(doc, e1, e2, dimensionSelector, colorManager);
		DimensionHighlighter h = new DimensionHighlighter();
		e1.setHighlighter(h);
		e1.addMouseListener(da);
		da.setText();
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
		
		add(dimensionSelector);
		add(inner);
		
		setSize(new java.awt.Dimension(700,550));
		setVisible(true);
	}
	
	private void setMenus()
	{
		JMenuBar mb = new JMenuBar();
				
		mb.add(fileMenu(new JMenu("File")));
		mb.add(editMenu(new JMenu("Edit")));
		
		e1.setComponentPopupMenu((JPopupMenu)editMenu(new JPopupMenu()));
		
		this.setJMenuBar(mb);
	}
	
	private <T extends JComponent> T fileMenu(T m)
	{
		JMenuItem mi;
		
		mi = new JMenuItem("New");
		mi.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent arg0) {
				da.newDoc();
			}
		});
		m.add(mi);

		mi = new JMenuItem("Open");
		mi.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				JOptionPane.showMessageDialog(null, "not implemented yet");
			}
		});
		m.add(mi);

		mi = new JMenuItem("Save");
		mi.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				JOptionPane.showMessageDialog(null, "not implemented yet");
			}
		});
		m.add(mi);

		mi = new JMenuItem("Exit");
		mi.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				setVisible(false);
				System.exit(0);
			}
		});
		m.add(mi);
		
		return m;
	}

	private JComponent editMenu(JComponent m)
	{
		JMenuItem mi;
		
		mi = new JMenuItem("Add Text");
		mi.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent arg0) {
				new AddTextDialog(da);
			}
		});
		m.add(mi);

		mi = new JMenuItem("Create Choice");
		mi.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				new CreateChoiceDialog(da);
			}
		});
		m.add(mi);

		mi = new JMenuItem("Remove Choice");
		mi.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				new RemoveChoiceDialog(da);
			}
		});
		m.add(mi);

		mi = new JMenuItem("Add Alternative");
		mi.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				new AddAlternativeDialog(da);
			}
		});
		m.add(mi);

		mi = new JMenuItem("Remove Alternative");
		mi.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				new RemoveAlternativeDialog(da);
			}
		});
		m.add(mi);
		
		return m;
	}
}
