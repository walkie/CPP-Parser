package editor.model;

public class Part2
{
	private int start;
	private Part part;
	
	public Part2(int start, Part part)
	{
		this.part = part;
		this.start = start;
	}

	public int getStart()
	{
		return start;
	}

	public Part getPart()
	{
		return part;
	}
}
