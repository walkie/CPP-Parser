package editor.ui;

import java.awt.Color;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.util.Collection;
import java.util.Set;
import java.util.TreeSet;

import javax.swing.JEditorPane;
import javax.swing.JOptionPane;
import javax.swing.event.DocumentEvent;
import javax.swing.event.DocumentListener;
import javax.swing.text.BadLocationException;

import editor.AbstractVersionedObject;
import editor.Dimension;
import editor.VersionedObject;
import editor.util.AlternativeAdder;
import editor.util.AlternativeRemover;
import editor.util.ChoiceCreator;
import editor.util.ChoiceRemover;
import editor.util.TagSelector;
import editor.util.TextAdder;
import editor.util.TextGetter;

public class DocumentAdapter implements DocumentListener, MouseListener {

	private AbstractVersionedObject doc;
	private JEditorPane textBox;
	private JEditorPane stTextBox;
	private DimensionSelector dimensionSelecter;
	
	public DocumentAdapter()
	{
	}
	
	public void setDocument(AbstractVersionedObject doc, JEditorPane textBox, JEditorPane stTextBox, DimensionSelector dimensionSelecter)
	{
		this.doc = doc;
		this.textBox = textBox;
		this.stTextBox = stTextBox;
		this.dimensionSelecter = dimensionSelecter;
	}
		
	public void changedUpdate(DocumentEvent e) {
		// TODO Auto-generated method stub
		
	}

	public void insertUpdate(DocumentEvent e) {
		// TODO Auto-generated method stub
		
	}

	public void removeUpdate(DocumentEvent e) {
		// TODO Auto-generated method stub
		
	}

	public void mouseClicked(MouseEvent e) {
		// TODO Auto-generated method stub
		if (e.getButton() == MouseEvent.BUTTON1)
		{
		}
	}

	public void setText()
	{
		setText(doc);
	}
	
	public void setText(AbstractVersionedObject doc2)
	{
		TextGetter tg = new TextGetter();
		doc2.visit(tg);
		Collection<TextGetter.Line> lines = tg.getLines();
		
		DimensionHighlighter h = null;
		boolean hasHighlighter = true;
		try 
		{
			h = (DimensionHighlighter)textBox.getHighlighter();
		}
		catch (Exception e)
 		{
			hasHighlighter = false;
		}
		
		int r = 0;
		String str = "";
		for (TextGetter.Line line : lines)
		{
			str += line.getText();
		}
		textBox.setText(str);
		if (hasHighlighter)
		{
			for (TextGetter.Line line : lines)
			{
				r = (r + 100) % 256;
				try {
					h.addHighlight(line.getStartPos(), line.getEndPos(), new Color(r,0,0));
				} catch (BadLocationException e) {
					e.printStackTrace();
				}
			}
		}
		
		stTextBox.setText(doc2.getStructuredText());
		dimensionSelecter.setDimensions(new Dimension(doc).getDimensions(), selectedTags);
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

	TreeSet<String> selectedTags = new TreeSet<String>();
	
	public void select(String tag) {
		selectedTags.add(tag);
		TagSelector ts = new TagSelector(doc);
		AbstractVersionedObject doc2 = ts.select(selectedTags);
		setText(doc2);
	}

	public void unselect(Set<String> dim) {
		selectedTags.removeAll(dim);
	}

	public void newDoc() {
		this.doc = new VersionedObject("");
		setText();
	}

	public void addText(String text)
	{
		int pos = textBox.getCaretPosition();
		TextAdder ta = new TextAdder(pos, text);
		
		doc = doc.transform(ta);
		setText();
	}

	public void createChoice(String tag)
	{
		int pos = textBox.getCaretPosition();
		ChoiceCreator cc = new ChoiceCreator(pos, tag);
		
		doc = doc.transform(cc);
		setText();		
	}

	public void removeChoice() 
	{
		int pos = textBox.getCaretPosition();
		ChoiceRemover cr = new ChoiceRemover(pos);
		
		doc = doc.transform(cr);
		setText();		
	}

	public void addAlternative(String tag, String text) 
	{
		int pos = textBox.getCaretPosition();
		AlternativeAdder aa = new AlternativeAdder(pos, tag, text);
		
		doc = doc.transform(aa);
		
		if (aa.suceeded())
			setText();	
		else
			JOptionPane.showMessageDialog(null, "Could not add alternative at cursor position.");
	}

	public void removerAlternative() 
	{
		int pos = textBox.getCaretPosition();
		AlternativeRemover ar = new AlternativeRemover(pos);
		
		doc = doc.transform(ar);
		setText();		
	}
}
