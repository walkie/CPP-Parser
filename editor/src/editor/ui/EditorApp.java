package editor.ui;

import javax.swing.WindowConstants;

import editor.*;
import editor.util.*;

public class EditorApp {
	private static final long serialVersionUID = 1L;

   /**
	 * @param args
	 */
	public static void main(String[] args) 
	{
		Editor e = new Editor(new DocumentAdapter());
				
		AbstractVersionedObject v = Temp.getTestDoc();
		Dimension d = new Dimension(v);
		
		e.setDimensionList(d.getDimensions());

		e.setTopDoc(v);
		e.setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE);
		e.showit();
	}
}
