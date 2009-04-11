package editor.util;

import editor.Choice;
import editor.AbstractVersionedObject;
import editor.Label;
import editor.VersionedObject;

public class Temp {
	public static AbstractVersionedObject getTestDoc()
	{
		VersionedObject p = new VersionedObject("");
		try {

//			Label l_ft = new Label(new String[] { "f", "t" });
//			Label l_45 = new Label(new String[] { "4", "5" });
			
			Label l_4 = new Label(new String[] { "4" });
			Label l_5 = new Label(new String[] { "5" });
	
			Label l_f = new Label(new String[] { "f" });
			Label l_t = new Label(new String[] { "t" });
			
			Choice c;
			
			p.addSubObject(new VersionedObject("class GoodApp {\n"));
			
			c = new Choice();
			c.addAlternative((Label)l_5.clone(), new VersionedObject("List<Job> jobs = new LinkedList<Job>();\n"));
			c.addAlternative((Label)l_4.clone(), new VersionedObject("List jobs = new LinkedList();\n"));
			p.addSubObject(c);
			
			p.addSubObject(new VersionedObject("void runJobs() {\n"));
			
			c = new Choice();
			c.addAlternative((Label)l_f.clone(), new VersionedObject(""));
			c.addAlternative((Label)l_t.clone(), new VersionedObject("int trialCount = 0;\n"));
			p.addSubObject(c);
			
			c = new Choice();
			c.addAlternative((Label)l_5.clone(), new VersionedObject(""));
			c.addAlternative((Label)l_4.clone(), new VersionedObject("Iterator it = jobs.iterator();\n"));
			p.addSubObject(c);
			
			c = new Choice();
			c.addAlternative((Label)l_5.clone(), new VersionedObject("for (Job j : jobs) {\n"));
			c.addAlternative((Label)l_4.clone(), new VersionedObject("while (it.hasNext()) {\n"));
			p.addSubObject(c);
			
			c = new Choice();
			c.addAlternative((Label)l_5.clone(), new VersionedObject(""));
			c.addAlternative((Label)l_4.clone(), new VersionedObject("Job j = (Job) it.next();\n"));
			p.addSubObject(c);
			
			p.addSubObject(new VersionedObject("j.run();\n"));
			
			c = new Choice();
			c.addAlternative((Label)l_f.clone(), new VersionedObject(""));
			c.addAlternative((Label)l_t.clone(), new VersionedObject("if (++trialCount > 5) break;\n"));
			p.addSubObject(c);
			
			p.addSubObject(new VersionedObject("}\n"));
			p.addSubObject(new VersionedObject("}\n"));
			p.addSubObject(new VersionedObject("}\n"));

		} catch (CloneNotSupportedException e) {
			e.printStackTrace();
		}
		return p;
	}
}
