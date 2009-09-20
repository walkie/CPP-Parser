package editor.ui.backup;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.util.Collection;

import javax.swing.BoxLayout;
import javax.swing.JEditorPane;
import javax.swing.JOptionPane;
import javax.swing.event.DocumentEvent;
import javax.swing.event.DocumentListener;
import javax.swing.text.BadLocationException;

import editor.model.Dim;
import editor.model.Document;
import editor.controller.VersionedDocument;

public class DocumentAdapter implements DocumentListener, MouseListener
{
	private VersionedDocument doc;
	private JEditorPane textBox;
	private DimensionSelector dimensionSelecter;
	private ColorManager colorManager;
	
	public DocumentAdapter()
	{
		this.doc = new VersionedDocument();
	}
	
	public void setDocument(VersionedDocument doc, JEditorPane textBox, DimensionSelector dimensionSelecter, ColorManager colorManager)
	{
		this.doc = doc;
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
			
//			doc.addText(pos, text);
		}
	}

	public void removeUpdate(DocumentEvent e) 
	{
		if (!inSetText)
		{
//			doc.removeText(e.getOffset(), e.getLength());
		}
	}

	public void mouseClicked(MouseEvent e) 
	{
		if (e.getButton() == MouseEvent.BUTTON1 && e.isControlDown())
		{		
			int pos = textBox.getCaretPosition();
//			String[] texts = doc.getTextWithHidden(pos);
//			if (texts != null)
//			{
//				// TODO: Move to own class
//				final javax.swing.JFrame d = new javax.swing.JFrame();
//				javax.swing.JPanel p = new javax.swing.JPanel();
//				p.setLayout(new BoxLayout(p, BoxLayout.Y_AXIS));
//				
//				for (String text : texts)
//				{
//					p.add(new javax.swing.JLabel(text));
//				}
//				
//				d.add(p);
//				d.setLocationRelativeTo(textBox);
//				try
//				{
//					java.awt.Point pt = textBox.modelToView(pos).getLocation();
//					java.awt.Point pt2 = textBox.getLocationOnScreen();
//					
//					d.setLocation(pt.x+pt2.x, pt.y+pt2.y);
//				}
//				catch (BadLocationException e1)
//				{
//					e1.printStackTrace();
//				}
//				d.setUndecorated(true);
//				d.setSize(new java.awt.Dimension(200,200));
//				d.setVisible(true);
//				
//				new javax.swing.Timer(10000, new ActionListener() {
//					public void actionPerformed(ActionEvent e)
//					{
//						d.setVisible(false);
//					}
//				}).start();
//			}
		}
	}

	public void setText()
	{
		inSetText = true;
		
//		colorManager.setDimensions(doc.getDimensions());
//		dimensionSelecter.setDimensions(doc.getDimensions(), doc.getSelectedTags());

//		Collection<TextPart> parts = doc.getTextParts();

//		textBox.setText(getText(parts));
//		setHighlights(parts);

		inSetText = false;
	}

//	private void setHighlights(Collection<TextPart> parts) {
//		for (TextPart part : parts)
//		{
//			if (!part.isAlt())
//				continue;
//
//			try {
//				DimensionHighlighter h = null;
//				h = (DimensionHighlighter)textBox.getHighlighter();
////				h.addHighlight(part.getStartPos(), part.getEndPos(), colorManager.getColor(part.getTag()));
//			} catch (Exception e) {
//				e.printStackTrace();
//			}
//		}
//	}

//	private String getText(Collection<TextPart> parts) {
//		String str = "";
////		for (TextPart part : parts)
////		{
//////			str += part.getText();
////		}
//		return str;
//	}
	
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
//		doc.select(tag);
		setText();
	}

	public void newDoc() 
	{
		this.doc = new VersionedDocument();
		setText();
	}

	public void addText(String text)
	{
		int pos = textBox.getCaretPosition();
//		doc.addText(pos, text);
		setText();
	}

	public void createChoice(String tag)
	{
		int start = textBox.getSelectionStart();
		int end = textBox.getSelectionEnd();
//		doc.createChoice(start, end, tag);
		setText();		
	}

	public void removeChoice() 
	{
		int pos = textBox.getCaretPosition();
//		doc.removeChoice(pos);
		setText();		
	}

	public void addAlternative(String tag, String text) 
	{
		int pos = textBox.getCaretPosition();
		
//		if (doc.addAlternative(pos, tag, text))
//			setText();	
//		else
//			JOptionPane.showMessageDialog(null, "Could not add alternative at cursor position.");
	}

	public void removeAlternative() 
	{
		int pos = textBox.getCaretPosition();
//		doc.removeAlternative(pos);
		setText();		
	}

	public void addTagToDim(String newTag, String oldTag, Dim dim)
	{
//		doc.addTagToDim(newTag, oldTag, dim);
		setText();
	}

	public void removeTagFromDim(String tag, Dim dim)
	{
//		doc.removeTagFromDim(tag, dim);
		setText();
	}

	public void removeDimension(Dim dim)
	{
//		doc.removeDimension(dim);
		setText();
	}

	public void debugPrint()
	{
//		doc.debugPrint();
	}


	public void debugPrintTags()
	{
//		for (String tag : doc.getDocument().getObject().tags())
//		{
//			System.out.print(tag);
//		}
//		System.out.println();
	}

//	public Document getDocument()
//	{
//		return doc.getDocument();
//	}
}
