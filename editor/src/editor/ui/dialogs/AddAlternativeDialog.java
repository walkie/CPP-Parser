package editor.ui.dialogs;

import java.awt.FlowLayout;
import java.awt.GridLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.JButton;
import javax.swing.JDialog;
import javax.swing.JPanel;
import javax.swing.JTextField;

import editor.ui.DocumentAdapter;

public class AddAlternativeDialog extends JDialog 
{
	private static final long serialVersionUID = 1L;
	DocumentAdapter documentAdapter;
	JTextField textBox;
	JTextField tagTextBox;
	JButton okButton;
	JButton cancelButton;

	public AddAlternativeDialog(DocumentAdapter da)
	{
		documentAdapter = da;
		tagTextBox = new JTextField();
		textBox = new JTextField();
		okButton = new JButton("Ok");
		cancelButton = new JButton("Cancel");
		
		okButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				documentAdapter.addAlternative(tagTextBox.getText(), textBox.getText()); 
				setVisible(false);
			}
		});

		cancelButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				setVisible(false);
			}
		});
		
		JPanel p = new JPanel();
		p.setLayout(new GridLayout(3,1));
		p.add(tagTextBox);
		p.add(textBox);
		
		JPanel p2 = new JPanel();
		
		p2.setLayout(new FlowLayout());
		
		p2.add(okButton);
		p2.add(cancelButton);
		p.add(p2);
		
		add(p);

		setSize(200,100);
		setTitle("Add Alternative");
		setVisible(true);
	}
}
