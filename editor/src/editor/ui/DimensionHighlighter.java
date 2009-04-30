package editor.ui;

import java.awt.Color;
import java.awt.Graphics;
import java.awt.Rectangle;
import java.awt.Shape;

import javax.swing.text.BadLocationException;
import javax.swing.text.DefaultHighlighter;
import javax.swing.text.JTextComponent;
import javax.swing.text.LayeredHighlighter;
import javax.swing.text.Position;
import javax.swing.text.View;

public class DimensionHighlighter extends DefaultHighlighter
{
	public DimensionHighlighter()
	{
	}

	public Object addHighlight(int p0, int p1, Color c) throws BadLocationException
	{
		return addHighlight(p0, p1, new DimensionHighlightPainter(c));
	}

	public static class DimensionHighlightPainter extends LayeredHighlighter.LayerPainter
	{
		protected Color color;

		public DimensionHighlightPainter(Color c)
		{
			color = c;
		}

	    public void paint(Graphics g, int offs0, int offs1, Shape bounds, JTextComponent c)
	    {
	    }

	    public Shape paintLayer(Graphics g, int offs0, int offs1, Shape bounds, JTextComponent c, View view)
	    {
			g.setColor(color == null ? c.getSelectionColor() : color);
			
			Rectangle alloc = null;
			if (offs0 == view.getStartOffset() && offs1 == view.getEndOffset())
			{
				if (bounds instanceof Rectangle)
				{
					alloc = (Rectangle) bounds;
				}
				else
				{
					alloc = bounds.getBounds();
				}
			}
			else
			{
				try
				{
					Shape shape = view.modelToView(offs0, Position.Bias.Forward, offs1, Position.Bias.Backward, bounds);
					alloc = (shape instanceof Rectangle) ? (Rectangle) shape : shape.getBounds();
				}
				catch (BadLocationException e) 
				{
					return null;
				}
			}
			
			g.fillRect(alloc.x, alloc.y, alloc.width, alloc.height);
			return alloc;
		}
	}
}
