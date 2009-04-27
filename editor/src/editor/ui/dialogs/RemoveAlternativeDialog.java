package editor.ui.dialogs;

import java.awt.FlowLayout;
import java.awt.GridLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.JButton;
import javax.swing.JDialog;
import javax.swing.JPanel;
import editor.ui.DocumentAdapter;

public class RemoveAlternativeDialog extends JDialog
{
	private static final long serialVersionUID = 1L;
	DocumentAdapter documentAdapter;
	JButton okButton;
	JButton cancelButton;

	public RemoveAlternativeDialog(DocumentAdapter da)
	{
		documentAdapter = da;
		okButton = new JButton("Ok");
		cancelButton = new JButton("Cancel");
		
		okButton.addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(ActionEvent arg0) {
				documentAdapter.removerAlternative(); 
				setVisible(false);
			}
		});

		cancelButton.addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(ActionEvent e) {
				setVisible(false);
			}
		});
		
		JPanel p = new JPanel();
		p.setLayout(new GridLayout(1,1));
		
		JPanel p2 = new JPanel();
		
		p2.setLayout(new FlowLayout());
		
		p2.add(okButton);
		p2.add(cancelButton);
		p.add(p2);
		
		add(p);

		setSize(200,100);
		setTitle("Remove Alternative");
		setVisible(true);
	}
}
