package editor.ui;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;
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

public class Adapter implements DocumentListener, KeyListener
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
		if (inSetText)
			return;

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
		if (inSetText)
			return;

		int offset = e.getOffset();
		int length = e.getLength();
		Debug.print("removed: " + offset + " " + length);

		doc.removeText(offset, length);

		Debug.print("removed: " + doc.debugGetText());
		
		view.applyView();
	}
	
	public ActionListener createChoiceListener()
	{
		return new ActionListener()
		{
			public void actionPerformed(ActionEvent e)
			{
				createChoice();
			}
		};
	}
	
	private void createChoice()
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
	
	public ActionListener removeChoiceListener()
	{
		return new ActionListener()
		{
			public void actionPerformed(ActionEvent e)
			{
				removeChoice();
			}
		};
	}

	private void removeChoice()
	{
		int start = editor.getCaretPosition();
		Debug.print("remove choice: " + start);

		doc.removeChoice(start);
		
		setText();
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
					Dim dim = doc.addAlternative(pos);
					
					ds.updateDim(dim);
					
					setText();
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

		editor.setText(doc.getText());
		
		Collection<TextAttr> parts = doc.getTextParts();

		setHighlights(parts);

		inSetText = false;
	}

	private void setHighlights(Collection<TextAttr> parts) 
	{
		DimensionHighlighter h = null;
		
		h = (DimensionHighlighter)editor.getHighlighter();
		h.removeAllHighlights();

		if (h != null)
		{
			for (TextAttr part : parts)
			{
				try
				{
					h.addHighlight(part.getStartPos(), part.getEndPos(), colorManager.getColor(part.getDimName()));
				}
				catch (BadLocationException e)
				{
					e.printStackTrace();
				}
			}
		}
	}

	public void keyPressed(KeyEvent e)
	{
	}

	public void keyReleased(KeyEvent e)
	{
	}

	public void keyTyped(KeyEvent e)
	{
		if (e.isAltDown() && e.getKeyChar() == 'c' || e.getKeyChar() == 'C')
		{
			if (e.isShiftDown())
			{
				removeChoice();
			}
			else				
			{
				createChoice();
			}
		}
	}
}
