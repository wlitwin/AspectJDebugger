package debugger;

import java.util.Scanner;

/**
 * The main break command. Stops execution when the specified
 * method is about to run. Control is then transfered to the
 * Debugger prompt.
 */
class BreakpointCommand implements ICommand {
	public boolean matches(String input) {
		return input.toLowerCase().equals("break");
	}

	public String getHelp() {
		return "stop execution when the specified " + 
			"method is called";
	}

	public String getCommand() {
		return "break class.method";
	}

	public boolean doWork(Scanner in) {
		String line = in.nextLine().trim();
		String[] classAndMethod = line.split("\\.");
		if (classAndMethod.length < 2) {
			Debugger.errorln("Invalid, usage is - break class.method");	
			return false;
		}

		String clazz = classAndMethod[0];
		String method = classAndMethod[1];

		if (!ClassUtils.isValidClass(clazz)) {
			Debugger.errorln("Couldn't find class: " + clazz);
			return false;
		}

		if (!ClassUtils.isValidMethod(clazz, method)) {
			Debugger.errorln("Couldn't find method: " + method);
			return false;
		}

		String combined = clazz + "." + method;
		if (Breakpoint.breakpoints.contains(combined)) {
			Debugger.println("Removed breakpoint: " + combined);	
			Breakpoint.breakpoints.remove(combined);
		} else {
			Debugger.println("Added breakpoint: " + combined);
			Breakpoint.breakpoints.add(combined);
		}

		return false;
	}
}
