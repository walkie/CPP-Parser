package editor.ui.dialogs;

import java.awt.FlowLayout;
import java.awt.GridLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.JButton;
import javax.swing.JDialog;
import javax.swing.JPanel;
import javax.swing.JTextField;

import editor.Dimension;

public class CreateTagDialog extends JDialog {
	private static final long serialVersionUID = 1L;
	
	private final Dimension dim;
	JTextField textBox;
	JButton okButton;
	JButton cancelButton;
	
	public CreateTagDialog(Dimension dim)
	{
		this.dim = dim;
	
		textBox = new JTextField();

		okButton = new JButton("Ok");
		cancelButton = new JButton("Cancel");

		setUI();
	}
	
	public void setUI()
	{
		okButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				dim.addTag(textBox.getText());
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
		p.add(textBox);
		
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
