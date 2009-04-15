package editor.util;

import editor.Choice;
import editor.AbstractVersionedObject;
import editor.Label;
import editor.VersionedDocumentBuilder;
import editor.VersionedObject;

public class Temp 
{
	public static AbstractVersionedObject getTestDoc()
	{
		int i = 0;
		
		VersionedDocumentBuilder vdb = new VersionedDocumentBuilder();
		
		vdb.addText("class GoodApp {\n");

		vdb.addText("List<Job> jobs = new LinkedList<Job>();\n");
		i = vdb.size() - 1;
		vdb.createChoice(i, "5");
		vdb.addAlternative(i, "4", "List jobs = new LinkedList();\n");

		vdb.addText("void runJobs() {\n");

		vdb.addText("");
		i = vdb.size() - 1;
		vdb.createChoice(i, "f");
		vdb.addAlternative(i, "t", "int trialCount = 0;\n");
	
		vdb.addText("");
		i = vdb.size() - 1;
		vdb.createChoice(i, "5");
		vdb.addAlternative(i, "4", "Iterator it = jobs.iterator();\n");

		vdb.addText("for (Job j : jobs) {\n");
		i = vdb.size() - 1;
		vdb.createChoice(i, "5");
		vdb.addAlternative(i, "4", "while (it.hasNext()) {\n");

		vdb.addText("");
		i = vdb.size() - 1;
		vdb.createChoice(i, "5");
		vdb.addAlternative(i, "4", "Job j = (Job) it.next();\n");

		vdb.addText("j.run();\n");

		vdb.addText("");
		i = vdb.size() - 1;
		vdb.createChoice(i, "f");
		vdb.addAlternative(i, "t", "if (++trialCount > 5) break;\n");

		vdb.addText("}\n");
		vdb.addText("}\n");
		vdb.addText("}\n");

		return vdb.getVersionedDocument();
	}
}

