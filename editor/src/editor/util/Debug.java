package editor.util;

public class Debug
{
	public static boolean DEBUG = false;
	
	public static void print(String s)
	{
		if (DEBUG)
			System.out.println(s);
	}
}
