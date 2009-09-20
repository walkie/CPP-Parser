package editor.ui;

import java.awt.Dimension;
import java.util.ArrayList;

import javax.swing.JEditorPane;
import javax.swing.JScrollPane;
import javax.swing.WindowConstants;

import editor.util.Debug;

public class EditorApp 
{
	private static final long serialVersionUID = 1L;

   /**
	 * @param args
	 */
	public static void main(String[] args) 
	{
		Debug.DEBUG = true;
		
		Editor e = new Editor();
		
		DimensionSelector ds = new DimensionSelector();
		JEditorPane t = new JEditorPane();
		
		Adapter adapter = new Adapter(ds, t);
		
		EditMenu editMenu = new EditMenu(adapter);
		
		t.setComponentPopupMenu(editMenu);
		
		t.getDocument().addDocumentListener(adapter);
		
		e.addRight(new JScrollPane(t));
		e.addLeft(ds);
				
		e.setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE);
		e.setVisible(true);
	}
}
