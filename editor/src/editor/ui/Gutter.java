package editor.ui;

import java.awt.Color;
import java.awt.Dimension;

import javax.swing.JPanel;

public class Gutter extends JPanel
{
	private static final long serialVersionUID = 1L;

	public Gutter()
	{
		setBackground(Color.LIGHT_GRAY);
		setMaximumSize(new Dimension(20, Integer.MAX_VALUE));
	}
}
