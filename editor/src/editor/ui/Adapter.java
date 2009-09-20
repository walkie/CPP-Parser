package editor.ui;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.ArrayList;
import java.util.Collection;

import javax.swing.JEditorPane;
import javax.swing.JOptionPane;
import javax.swing.event.DocumentEvent;
import javax.swing.event.DocumentListener;
import javax.swing.text.BadLocationException;

import sun.reflect.generics.reflectiveObjects.NotImplementedException;

import editor.model.Dim;
import editor.model.Document;
import editor.model.exceptions.NoChoiceException;
import editor.ui.backup.DimensionHighlighter;
import editor.util.Debug;
import editor.util.TextAttr;

public class Adapter implements DocumentListener
{
	final Document doc;
	final DimensionSelector ds;
	final View view;
	final JEditorPane editor;
	final ColorManager colorManager;
	
	public Adapter(DimensionSelector ds, JEditorPane editor)
	{
		this.ds = ds;
		this.editor = editor;
		this.doc = new Document(this);
		this.view = new View(doc, editor);
		this.colorManager = new ColorManager();
	}
	
	public void changedUpdate(DocumentEvent e)
	{
		throw new NotImplementedException();
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
				
				Dim dim = doc.createChoice(ds.getSelectedDim(), start, end);
				
				ds.addDimension(dim.getName(), dim.getTags());
				
				setText();
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
				
				try
				{
					doc.addAlternative(pos);
				}
				catch (NoChoiceException e1)
				{
					JOptionPane.showMessageDialog(editor, "No choice here", "Error", JOptionPane.ERROR_MESSAGE);
				}
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
	
	boolean inSetText = false;
	public void setText()
	{
		inSetText = true;
		
		colorManager.setDimensions(doc.getDimensions());
		//ds.setDimensions(doc.getDimensions(), doc.getSelectedTags());

		Collection<TextAttr> parts = doc.getTextParts();

//		editor.setText(getText(parts));
		setHighlights(parts);

		inSetText = false;
	}

//	private String getText(Collection<TextAttr> parts) {
//		String str = "";
//		for (TextAttr part : parts)
//		{
//			str += part.getText();
//		}
//		return str;
//	}
	
	private void setHighlights(Collection<TextAttr> parts) {
		for (TextAttr part : parts)
		{
//			if (!part.isAlt())
//				continue;

			try {
				DimensionHighlighter h = null;
				h = (DimensionHighlighter)editor.getHighlighter();
				h.addHighlight(part.getStartPos(), part.getEndPos(), colorManager.getColor(part.getDimName()));
			} catch (Exception e) {
				e.printStackTrace();
			}
		}
	}
}
