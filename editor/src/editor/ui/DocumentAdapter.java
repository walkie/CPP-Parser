package editor.ui;

import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.util.Collection;
import java.util.Set;
import java.util.TreeSet;

import javax.swing.JEditorPane;
import javax.swing.JOptionPane;
import javax.swing.event.DocumentEvent;
import javax.swing.event.DocumentListener;

import editor.AbstractVersionedObject;
import editor.Dimension;
import editor.VersionedObject;
import editor.util.AlternativeAdder;
import editor.util.AlternativeRemover;
import editor.util.ChoiceCreator;
import editor.util.ChoiceRemover;
import editor.util.TagSelector;
import editor.util.TextAdder;

public class DocumentAdapter implements DocumentListener, MouseListener {

	private AbstractVersionedObject doc;
	private JEditorPane textBox;
	private DimensionSelector dimensionSelecter;
	private ColorManager colorManager;
	
	public DocumentAdapter()
	{
	}
	
	public void setDocument(AbstractVersionedObject doc, JEditorPane textBox, DimensionSelector dimensionSelecter, ColorManager colorManager)
	{
		this.doc = doc;
		this.textBox = textBox;
		this.dimensionSelecter = dimensionSelecter;
		this.colorManager = colorManager;
	}
		
	public void changedUpdate(DocumentEvent e) 
	{
	}

	public void insertUpdate(DocumentEvent e)
	{
//		int pos = e.getOffset();
//		String text = "";
//		try 
//		{
//			text = e.getDocument().getText(e.getOffset(), e.getOffset()+e.getLength());
//		}
//		catch (BadLocationException e1) 
//		{
//			e1.printStackTrace();
//			return;
//		}
//		TextAdder ta = new TextAdder(pos, text);
//		doc = doc.transform(ta);
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
		Dimension d = new Dimension(doc);
		colorManager.setDimensions(d.getDimensions());
		TagSelector ts = new TagSelector(selectedTags);
		doc.visit(ts);
		Collection<TagSelector.Line> lines = ts.getLines();
		
		String str = "";
		for (TagSelector.Line line : lines)
		{
			str += line.getText();
		}
		textBox.setText(str);
		for (TagSelector.Line line : lines)
		{
			if (!line.isAlt())
				continue;

			try {
				DimensionHighlighter h = null;
				h = (DimensionHighlighter)textBox.getHighlighter();
				h.addHighlight(line.getStartPos(), line.getEndPos(), colorManager.getColor(line.getLabel()));
			} catch (Exception e) {
				e.printStackTrace();
			}
		}
		
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
		setText();
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
