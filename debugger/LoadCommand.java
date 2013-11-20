package debugger;

import java.io.*;
import javax.tools.JavaCompiler;
import javax.tools.ToolProvider;
import javax.tools.JavaFileObject;
import javax.tools.StandardJavaFileManager;
import java.util.Scanner;

public class LoadCommand implements ICommand {
	static {
		Debugger.commands.add(new LoadCommand());
	}

	public boolean matches(String input) {
		return input.toLowerCase().equals("load");
	}

	public String getHelp() {
		return "load a class from a file";
	}

	public String getCommand() {
		return "load path";
	}

	public boolean doWork(Scanner in) {
		String path = in.nextLine().trim();

		File file = new File(path);
		if (!file.exists()) {
			Debugger.errorln("File doesn't exist");
			return false;
		}

		JavaCompiler jc = ToolProvider.getSystemJavaCompiler();
		StandardJavaFileManager fm = jc.getStandardFileManager(null, null, null);
		Iterable<? extends JavaFileObject> it = fm.getJavaFileObjects(file);
		boolean compiled = jc.getTask(null, fm, null, null, null, it).call();
		if (!compiled) {
			Debugger.errorln("Failed to compile file: " + path);
			return false;
		} else {
			try {
				Class.forName(path.split("\\.")[0]);
			} catch (ClassNotFoundException cnfe) {
				Debugger.errorln("LoadCommand: Shouldn't happen");	
			}
			Debugger.println("Successfully compiled");
		}

		return false;
	}
}
