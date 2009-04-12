package editor.ui;

import java.awt.Font;
import java.awt.GridLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.Collection;
import java.util.Set;

import javax.swing.ButtonGroup;
import javax.swing.JEditorPane;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JRadioButton;
import javax.swing.JScrollPane;

import editor.AbstractVersionedObject;

public class Editor extends JFrame {
	private static final long serialVersionUID = 1L;
	private final JEditorPane e1 = new JEditorPane();
	private final JEditorPane e2 = new JEditorPane();
	private JPanel dimPanel = null;
	private DocumentAdapter da;
	
	public Editor(DocumentAdapter da)
	{
		this.da = da;
	}
	
	private class RBLChecked implements ActionListener
	{
		DocumentAdapter da;
		String tag;
		
		public RBLChecked(DocumentAdapter da, String tag)
		{
			this.da = da;
			this.tag = tag;
		}
		
		@Override
		public void actionPerformed(ActionEvent e) {
			da.select(tag);
		}
	}
	
	public void setDimesionList(Collection<Set<String>> dimensions) {
		dimPanel = new JPanel();
		
		for (Set<String> d : dimensions)
		{
			JPanel p = new JPanel();
			p.add(new JLabel("Dimension"));
			ButtonGroup g = new ButtonGroup();
			for (String t : d)
			{
				JRadioButton r = new JRadioButton(t);
				r.addActionListener(new RBLChecked(da, t));
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
		da.setDocument(doc, e1);
		da.setText();
		e1.addMouseListener(da);
	}

	public void showit()
	{
		GridLayout layout  = new GridLayout(1,2);
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
		
		setSize(new java.awt.Dimension(500,500));
		setVisible(true);
	}
}
