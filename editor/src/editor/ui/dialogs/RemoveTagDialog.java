package editor.ui.dialogs;

import java.awt.FlowLayout;
import java.awt.GridLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.JButton;
import javax.swing.JDialog;
import javax.swing.JLabel;
import javax.swing.JPanel;

import editor.model.Dim;
import editor.ui.backup.DocumentAdapter;

public class RemoveTagDialog extends JDialog {
	private static final long serialVersionUID = 1L;
	
	private final String tag;
	private final Dim dim;
	private final DocumentAdapter da;
	private final JButton okButton;
	private final JButton cancelButton;
	
	public RemoveTagDialog(String tag, Dim dim, DocumentAdapter da)
	{
		this.tag = tag;
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
				da.removeTagFromDim(tag, dim);
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
