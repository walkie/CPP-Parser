package editor.model;

public interface DocTree
{
	public void replace(Obj oldObj, Obj newObj);
	public void remove(Obj obj);
	public Choice findChoice();
}
