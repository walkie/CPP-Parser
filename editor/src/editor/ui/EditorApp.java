package editor.ui;

import editor.*;
import editor.util.*;

public class EditorApp {
	private static final long serialVersionUID = 1L;

   /**
	 * @param args
	 */
	public static void main(String[] args) {
		Editor e = new Editor();
				
		AbstractVersionedObject v = Temp.getTestDoc();
		Dimension d = new Dimension(v);
		
		e.setDimesionList(d.getDimensions());

		e.setTopDoc(v.getText());
		e.setBottomText(v.getStructuredText());
		e.showit();
	}
}
