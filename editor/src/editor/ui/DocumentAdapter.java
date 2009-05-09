package editor.ui;

import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.util.Collection;
import java.util.Set;

import javax.swing.JEditorPane;
import javax.swing.JOptionPane;
import javax.swing.event.DocumentEvent;
import javax.swing.event.DocumentListener;
import javax.swing.text.BadLocationException;

import editor.AbstractVersionedObject;
import editor.VersionedDocument;
import editor.util.TagSelector;

public class DocumentAdapter implements DocumentListener, MouseListener {

	private VersionedDocument doc;
	private JEditorPane textBox;
	private DimensionSelector dimensionSelecter;
	private ColorManager colorManager;
	
	public DocumentAdapter()
	{
		this.doc = new VersionedDocument();
	}
	
	public void setDocument(AbstractVersionedObject doc, JEditorPane textBox, DimensionSelector dimensionSelecter, ColorManager colorManager)
	{
		this.doc.setDocument(doc);
		this.textBox = textBox;
		this.dimensionSelecter = dimensionSelecter;
		this.colorManager = colorManager;
	}
		
	public void changedUpdate(DocumentEvent e) 
	{
	}

	boolean inSetText = false;
	public void insertUpdate(DocumentEvent e)
	{
		if (!inSetText)
		{
			int pos = e.getOffset();
			String text = "";
			try 
			{
				text = e.getDocument().getText(e.getOffset(), e.getLength());
			}
			catch (BadLocationException e1) 
			{
				e1.printStackTrace();
				return;
			}
			
			doc.addText(pos, text);
		}
	}

	public void removeUpdate(DocumentEvent e) 
	{
		if (!inSetText)
		{
			doc.removeText(e.getOffset(), e.getLength());
		}
	}

	public void mouseClicked(MouseEvent e) 
	{
		if (e.getButton() == MouseEvent.BUTTON1)
		{		
			AbstractVersionedObject c = doc.getChoice(textBox.getCaretPosition());
			if (c != null)
			{
//				TagSelector ts = new TagSelector(doc.getSelectedTags());
//				c.visit(ts);
				
//				Collection<TagSelector.TextPart> parts = ts.getTextParts();
				
//				for (TagSelector.TextPart part : parts)
//				{
//					System.out.println("ALT: " + part.getText().replace("\n", ""));
//				}
			}
		}
	}

	public void setText()
	{
		inSetText = true;
		
		colorManager.setDimensions(doc.getDimensions());
		Collection<TagSelector.TextPart> parts = doc.getTextParts();
		
		String str = "";
		for (TagSelector.TextPart part : parts)
		{
			str += part.getText();
		}
		textBox.setText(str);
		for (TagSelector.TextPart part : parts)
		{
			if (!part.isAlt())
				continue;

			try {
				DimensionHighlighter h = null;
				h = (DimensionHighlighter)textBox.getHighlighter();
				h.addHighlight(part.getStartPos(), part.getEndPos(), colorManager.getColor(part.getLabel()));
			} catch (Exception e) {
				e.printStackTrace();
			}
		}
		
		dimensionSelecter.setDimensions(doc.getDimensions(), doc.getSelectedTags());

		inSetText = false;
	}
	
	public void mouseEntered(MouseEvent e) {
		// TODO Auto-generated method stub
		
	}

	public void mouseExited(MouseEvent e) {
		// TODO Auto-generated method stub
		
	}

	public void mousePressed(MouseEvent e) {
		// TODO Auto-generated method stub
		
	}

	public void mouseReleased(MouseEvent e) {
		// TODO Auto-generated method stub
		
	}

	public void select(String tag) 
	{
		doc.select(tag);
		setText();
	}

	public void unselect(Set<String> dim) 
	{
		doc.unselect(dim);
	}

	public void newDoc() 
	{
		this.doc = new VersionedDocument();
		setText();
	}

	public void addText(String text)
	{
		int pos = textBox.getCaretPosition();
		doc.addText(pos, text);
		setText();
	}

	public void createChoice(String tag)
	{
		int pos = textBox.getCaretPosition();
		doc.createChoice(pos, tag);
		setText();		
	}

	public void removeChoice() 
	{
		int pos = textBox.getCaretPosition();
		doc.removeChoice(pos);
		setText();		
	}

	public void addAlternative(String tag, String text) 
	{
		int pos = textBox.getCaretPosition();
		
		if (doc.addAlternative(pos, tag, text))
			setText();	
		else
			JOptionPane.showMessageDialog(null, "Could not add alternative at cursor position.");
	}

	public void removeAlternative() 
	{
		int pos = textBox.getCaretPosition();
		doc.removeAlternative(pos);
		setText();		
	}
}
