package editor.ui;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.ArrayList;

import javax.swing.JEditorPane;
import javax.swing.JOptionPane;
import javax.swing.event.DocumentEvent;
import javax.swing.event.DocumentListener;
import javax.swing.text.BadLocationException;

import sun.reflect.generics.reflectiveObjects.NotImplementedException;

import editor.model.Dim;
import editor.model.Document;
import editor.util.Debug;

public class Adapter implements DocumentListener
{
	final Document doc;
	final DimensionSelector ds;
	final View view;
	final JEditorPane editor;
	
	public Adapter(DimensionSelector ds, JEditorPane editor)
	{
		this.ds = ds;
		this.editor = editor;
		this.doc = new Document(this);
		this.view = new View(doc, editor);
	}
	
	public void changedUpdate(DocumentEvent e)
	{
		throw new NotImplementedException();
//		try
//		{
//			int offset = e.getOffset();
//			int length = e.getLength();
//			String text = e.getDocument().getText(offset, length);
//			Debug.print("changed: " + offset + " " + length + " " + text);
//
//			doc.removeText(offset, length);
//			doc.insertText(offset, text);
//
//			Debug.print("changed: " + doc.getText());
//		}
//		catch (BadLocationException e1)
//		{
//			e1.printStackTrace();
//		}
	}

	public void insertUpdate(DocumentEvent e)
	{
		try
		{
			int offset = e.getOffset();
			int length = e.getLength();
			String text = e.getDocument().getText(offset, length);
			Debug.print("inserted: " + offset + " " + length + " " + text);
			
			doc.insertText(offset, text);

			Debug.print("inserted: " + doc.debugGetText());
			
			view.applyView();
		}
		catch (BadLocationException e1)
		{
			e1.printStackTrace();
		}
	}

	public void removeUpdate(DocumentEvent e)
	{
		int offset = e.getOffset();
		int length = e.getLength();
		Debug.print("removed: " + offset + " " + length);

		doc.removeText(offset, length);

		Debug.print("removed: " + doc.debugGetText());
		
		view.applyView();
	}
	
	public ActionListener createChoice()
	{
		return new ActionListener()
		{
			public void actionPerformed(ActionEvent e)
			{
				int start = editor.getSelectionStart();
				int end = editor.getSelectionEnd();
				Debug.print("create choice: " + start + " " + end);
				
				if (start == end)
				{
					JOptionPane.showMessageDialog(editor, "Please select text");
					return;
				}
				
				Dim dim = doc.createChoice(start, end);
				
				ds.addDimension(dim.getName(), dim.getTags());
			}
		};
	}
	
	public ActionListener removeChoice()
	{
		return new ActionListener()
		{
			public void actionPerformed(ActionEvent e)
			{
				int start = editor.getSelectionStart();
				int end = editor.getSelectionEnd();
				Debug.print("remove choice: " + start + " " + end);

				doc.removeChoice(start);
			}
		};
	}

	public ActionListener addAlternative()
	{
		return new ActionListener()
		{
			public void actionPerformed(ActionEvent e)
			{
				// I would rather get the mouse cursor position here
				int pos = editor.getCaretPosition();
				Debug.print("add alt: " + pos);
				
				doc.addAlternative(pos);
			}
		};
	}
	
	public ActionListener removeAlternative()
	{
		return new ActionListener()
		{
			public void actionPerformed(ActionEvent e)
			{
				// I would rather get the mouse cursor position here
				int pos = editor.getCaretPosition();
				Debug.print("rem alt: " + pos);

				doc.removeAlternative(pos);
			}
		};
	}

	public ActionListener debugPrint()
	{
		return new ActionListener()
		{
			public void actionPerformed(ActionEvent e)
			{
				doc.debugPrint();
			}
		};
	}
}
