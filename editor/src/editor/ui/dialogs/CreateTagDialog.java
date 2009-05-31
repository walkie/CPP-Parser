package editor.ui.dialogs;

import java.awt.FlowLayout;
import java.awt.GridLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.JButton;
import javax.swing.JComboBox;
import javax.swing.JDialog;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JTextField;

import editor.Dimension;
import editor.ui.DocumentAdapter;

public class CreateTagDialog extends JDialog {
	private static final long serialVersionUID = 1L;
	
	private final Dimension dim;
	private final DocumentAdapter da;
	private final JTextField textBox;
	private final JButton okButton;
	private final JButton cancelButton;
	private final JComboBox list;
	
	public CreateTagDialog(Dimension dim, DocumentAdapter da)
	{
		this.dim = dim;
		this.da = da;
		
		textBox = new JTextField();
		list = new JComboBox(dim.tags().toArray());
		
		okButton = new JButton("Ok");
		cancelButton = new JButton("Cancel");

		setUI();
	}
	
	public void setUI()
	{
		okButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				if (textBox.getText().length() > 0)
				{
					da.addTagToDim(textBox.getText(), list.getSelectedItem().toString(), dim);
					setVisible(false);
				}
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
		JPanel p2 = new JPanel();
		p2.setLayout(new GridLayout(2,2));
		
		p2.add(new JLabel("Tag Name:"));
		p2.add(textBox);
		
		p2.add(new JLabel("Specializes:"));
		
		p2.add(list);
		
		p.add(p2);
		
		//
		JPanel p3 = new JPanel();
		p3.setLayout(new FlowLayout());
		
		p3.add(okButton);
		p3.add(cancelButton);
		p.add(p3);
		
		add(p);
		
		setSize(200,120);
		setVisible(true);
	}
}
