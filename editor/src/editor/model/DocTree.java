package editor.model;

import java.util.ArrayList;

public interface DocTree
{
	void replace(Obj oldObj, Obj newObj);
	void remove(Obj obj);
	Choice findChoice();
	int getBetween(int pos, int start, int end, ObjList objList);
	int removeBetween(int pos, int start, int end);
}
