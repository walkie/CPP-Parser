package editor.ui;

import java.awt.Color;
import java.awt.Graphics;
import java.awt.Rectangle;
import java.awt.Shape;
import java.util.Hashtable;

import javax.swing.text.BadLocationException;
import javax.swing.text.JTextComponent;

public class DocumentHighlighter implements javax.swing.text.Highlighter {

	class DocumentHighlight implements Highlight
	{
		int start;
		int end;
		
		public DocumentHighlight(int start, int end)
		{
			this.start = start;
			this.end = end;
		}
		
		@Override
		public int getEndOffset() {
			return end;
		}

		@Override
		public HighlightPainter getPainter() {
			return new DocumentHighlightPainter();
		}

		@Override
		public int getStartOffset() {
			return start;
		}

		public void setStartOffset(int start) {
			this.start = start;
		}

		public void setEndOffset(int end) {
			this.end = end;
		}
	}
	
	class DocumentHighlightPainter implements HighlightPainter
	{
		@Override
		public void paint(Graphics g, int start, int end, Shape bounds,
				JTextComponent c) {
			if (bounds != null)
			{
				Rectangle r = bounds.getBounds();
				g.setColor(new Color(0,0,255));
				g.drawRect(r.x, r.y, r.width, r.height);
			}
		}
	}

	final Hashtable<Object,DocumentHighlight> highlights = new Hashtable<Object,DocumentHighlight>();
	int tagNum = 0;
	HighlightPainter painter = new DocumentHighlightPainter();
	
	public DocumentHighlighter()
	{
	}
	 
	public Object addHighlight(int start, int end)
			throws BadLocationException {
		return addHighlight(start, end, new DocumentHighlightPainter());
	}

	@Override
	public Object addHighlight(int start, int end, HighlightPainter p)
			throws BadLocationException {
		Object tag = new Integer(++tagNum);
		highlights.put(tag, new DocumentHighlight(start, end));
		return tag;
	}

	@Override
	public void changeHighlight(Object tag, int start, int end)
			throws BadLocationException {
		DocumentHighlight h = highlights.get(tag);
		h.setStartOffset(start);
		h.setEndOffset(end);
	}

	@Override
	public void deinstall(JTextComponent c) {
	}

	@Override
	public Highlight[] getHighlights() {
		Highlight[] hs = new Highlight[highlights.size()];
		Object[] os = highlights.values().toArray();
		for (int i = 0; i < highlights.size(); i++)
		{
			hs[i] = (Highlight)os[i];
		}
		return hs;
	}

	JTextComponent c = null;
	@Override
	public void install(JTextComponent c) {
		this.c = c;
	}

	@Override
	public void paint(Graphics g) {
		for (Highlight h : getHighlights())
		{
			try {
				Rectangle p1 = c.modelToView(h.getStartOffset());
				Rectangle p2 = c.modelToView(h.getEndOffset());
				Shape s = new Rectangle(p1.x,p1.y,p2.x-p1.y,p2.y-p1.y+15);
				h.getPainter().paint(g, h.getStartOffset(), h.getEndOffset(), s, c);
			} catch (BadLocationException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
		}
	}

	@Override
	public void removeAllHighlights() {
		highlights.clear();
	}

	@Override
	public void removeHighlight(Object tag) {
		highlights.remove(tag);
	}

}
