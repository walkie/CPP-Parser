package editor.ui;

import javax.swing.JMenuItem;
import javax.swing.JPopupMenu;

import editor.util.Debug;


public class EditMenu extends JPopupMenu
{
	public EditMenu(Adapter adapter)
	{
		Debug.print("Popup menu created");
		
		JMenuItem mi;
		
		mi = new JMenuItem("Create Choice");
		mi.addActionListener(adapter.createChoice());
		add(mi);

		mi = new JMenuItem("Remove Choice");
		mi.addActionListener(adapter.removeChoice());
		add(mi);

		mi = new JMenuItem("Add Alternative");
		mi.addActionListener(adapter.addAlternative());
		add(mi);

		mi = new JMenuItem("Remove Alternative");
		mi.addActionListener(adapter.removeAlternative());
		add(mi);

		mi = new JMenuItem("Debug Print");
		mi.addActionListener(adapter.debugPrint());
		add(mi);
	}
}
