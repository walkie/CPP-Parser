package editor.ui.dialogs;

import java.awt.FlowLayout;
import java.awt.GridLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.JButton;
import javax.swing.JDialog;
import javax.swing.JLabel;
import javax.swing.JPanel;

import editor.Dimension;
import editor.ui.DocumentAdapter;

public class RemoveDimensionDialog extends JDialog {
	private static final long serialVersionUID = 1L;
	
	private final Dimension dim;
	private final DocumentAdapter da;
	private final JButton okButton;
	private final JButton cancelButton;
	
	public RemoveDimensionDialog(Dimension dim, DocumentAdapter da)
	{
		this.dim = dim;
		this.da = da;
				
		okButton = new JButton("Ok");
		cancelButton = new JButton("Cancel");

		setUI();
	}
	
	public void setUI()
	{
		okButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				da.removeDimension(dim);
				setVisible(false);
			}
		});

		cancelButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				setVisible(false);
			}
		});
		
		JPanel p = new JPanel();
		p.setLayout(new GridLayout(2,1));
		
		//
		p.add(new JLabel("Are you sure?"));
		
		//
		JPanel p2 = new JPanel();
		p2.setLayout(new FlowLayout());
		
		p2.add(okButton);
		p2.add(cancelButton);
		p.add(p2);
		
		add(p);
		
		setSize(200,100);
		setVisible(true);
	}
}
