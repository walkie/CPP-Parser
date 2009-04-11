package editor.ui;

import java.awt.Point;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.util.ArrayList;

import javax.swing.JEditorPane;
import javax.swing.event.DocumentEvent;
import javax.swing.event.DocumentListener;

import editor.util.*;

public class DocumentAdapter implements DocumentListener, MouseListener {

	private Tree<String> doc;
	private final JEditorPane textBox;
	private final DocumentHighlighter highlighter = new DocumentHighlighter();
	
	public DocumentAdapter(Tree<String> doc, JEditorPane textBox)
	{
		this.doc = doc;
		this.textBox = textBox;
		textBox.setHighlighter(highlighter);
	}
	
	public int switchAt(int i, int pos, Tree<String> d)
	{
		System.out.format("switchAt(i: %d, pos: %d)\n", i, pos);
		i += d.getValue().length();
		System.out.println(" A " + d.getValue());
		if (pos < i)
			return i;
		
		if (d.isSeq())
		{
			i = switchAt(i, pos, d.getChildren());
		}
		else
		{
			i = switchAt(i, pos, d.getChildren().get(0));
			if (pos < i)
				rot(d.getChildren());
		}
		
		return i;
	}
	
	private int switchAt(int i, int pos, ArrayList<Tree<String>> ts) {
		System.out.format("switchAt[](i: %d, pos: %d)\n", i, pos);
		for (Tree<String> t : ts)
		{
			i = switchAt(i, pos, t);
			if (pos < i)
				return i;
		}
		return i;
	}

	private void rot(ArrayList<Tree<String>> ts)
	{
		Tree<String> t = ts.remove(0);
		ts.add(t);
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
			int pos = textBox.viewToModel(new Point(e.getX(), e.getY()));
			this.switchAt(0, pos, doc);
			setText();
			int pos2 = textBox.viewToModel(new Point(e.getX(), e.getY()));
			textBox.setCaretPosition(pos2);
		}
	}

	public void setText()
	{
		ArrayList<String> ss = new ArrayList<String>(); 
		flatten(doc, ss);
		String str = "";
		for (int i = 0; i < ss.size(); i++)
		{
			String str2 = ss.get(i);
			str += str2;
		}

		textBox.setText(str);

//		choices.clear();
//		choiceIdx = 0;
//		str = "";
//		int start = 0, end = 0;
//		for (int i = 0; i < ss.size(); i++)
//		{
//			String str2 = ss.get(i);
//			end = start + str2.length();
//			if (start != end)
//			{
//				try {
//					highlighter.addHighlight(start, end);
//				} catch (BadLocationException e) {
//					e.printStackTrace();
//				}
//			}
//			start = end;
//			str += str2;
//		}
	}
	
	ArrayList<Integer> choices = new ArrayList<Integer>();
	int choiceIdx = 0;
	
	public void flatten(Tree<String> d, ArrayList<String> ss) {
		ss.add(d.getValue());
		System.out.format("XX %d %s", choiceIdx, d.getValue());
		choiceIdx++;
		for (Tree<String> t : d.getChildren())
		{
			flatten(t,ss);
			if (!d.isSeq())
			{
				choices.add(new Integer(choiceIdx));
				break;
			}
		}
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

}
