package editor.ui;

import javax.swing.WindowConstants;

import editor.controller.VersionedDocument;
import editor.util.*;

public class EditorApp {
	private static final long serialVersionUID = 1L;

   /**
	 * @param args
	 */
	public static void main(String[] args) 
	{
		Editor e = new Editor(new DocumentAdapter());
				
		VersionedDocument v = new VersionedDocument(Temp.getTestDoc());
		e.setDocument(v);
		e.setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE);
		e.showit();
	}
}
