package editor.ui;

import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.util.Set;
import java.util.TreeSet;

import javax.swing.JEditorPane;
import javax.swing.event.DocumentEvent;
import javax.swing.event.DocumentListener;

import editor.AbstractVersionedObject;
import editor.ChoiceCreator;
import editor.Dimension;
import editor.TagSelector;
import editor.VersionedObject;

public class DocumentAdapter implements DocumentListener, MouseListener {

	private AbstractVersionedObject doc;
	private JEditorPane textBox;
	private JEditorPane stTextBox;
	private Editor editor;
	
	public DocumentAdapter()
	{
	}
	
	public void setDocument(AbstractVersionedObject doc, JEditorPane textBox, JEditorPane stTextBox, Editor editor)
	{
		this.doc = doc;
		this.textBox = textBox;
		this.stTextBox = stTextBox;
		// don't need this if I have a control for the dimension selector
		this.editor = editor;
	}
		
	@Override
	public void changedUpdate(DocumentEvent e) {
		// TODO Auto-generated method stub
		
	}

	@Override
	public void insertUpdate(DocumentEvent e) {
		// TODO Auto-generated method stub
		
	}

	@Override
	public void removeUpdate(DocumentEvent e) {
		// TODO Auto-generated method stub
		
	}

	@Override
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
		textBox.setText(doc2.getText());
		stTextBox.setText(doc2.getStructuredText());
		editor.setDimesionList(new Dimension(doc2).getDimensions());
	}
	
	@Override
	public void mouseEntered(MouseEvent e) {
		// TODO Auto-generated method stub
		
	}

	@Override
	public void mouseExited(MouseEvent e) {
		// TODO Auto-generated method stub
		
	}

	@Override
	public void mousePressed(MouseEvent e) {
		// TODO Auto-generated method stub
		
	}

	@Override
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

	public void addText(String text) {
		((VersionedObject)doc).getSubObjects().add(new VersionedObject(text));
		setText();
	}

	public void createChoice(String tag)
	{
		int pos = textBox.getCaretPosition();
		ChoiceCreator cc = new ChoiceCreator(pos, tag);
		
		doc = doc.transform(cc);
		setText();		
	}
}
