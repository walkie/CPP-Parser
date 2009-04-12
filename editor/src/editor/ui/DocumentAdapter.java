package editor.ui;

import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;

import javax.swing.JEditorPane;
import javax.swing.event.DocumentEvent;
import javax.swing.event.DocumentListener;

import editor.AbstractVersionedObject;
import editor.TagSelector;

public class DocumentAdapter implements DocumentListener, MouseListener {

	private AbstractVersionedObject doc;
	private JEditorPane textBox;
	
	public DocumentAdapter()
	{
	}
	
	public void setDocument(AbstractVersionedObject doc, JEditorPane textBox)
	{
		this.doc = doc;
		this.textBox = textBox;
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
		textBox.setText(doc.getText());
	}
	
	public void setText(AbstractVersionedObject doc2)
	{
		textBox.setText(doc2.getText());
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

	public void select(String tag) {
		TagSelector ts = new TagSelector(doc);
		AbstractVersionedObject doc2 = ts.select(tag);
		setText(doc2);
	}
}
