package debugger;

import java.util.Scanner;

/**
 * Break on class field get. Stops execution when a specified class
 * field is about to be read. Control transfers to the Debugger
 * prompt.
 */
class BreakGetCommand implements ICommand {
	public boolean matches(String input) {
		return input.toLowerCase().equals("breakget");
	}

	public String getHelp() {
		return "break on a field get";
	}

	public String getCommand() {
		return "breakget class.field";
	}

	public boolean doWork(Scanner in) {
		String line = in.nextLine().trim();

		String[] classAndField = line.split("\\.");			
		if (classAndField.length < 2) {
			Debugger.errorln("Invalid, usage is - " + getCommand());
			return false;
		}

		String clazz = classAndField[0];
		String field = classAndField[1];

		if (!ClassUtils.isValidClass(clazz)) {
			Debugger.errorln("Couldn't find class: " + clazz);
			return false;
		}

		if (!ClassUtils.isValidField(clazz, field)) {
			Debugger.errorln("Couldn't find field: " + field);
			return false;
		}

		String combined = clazz + "." + field;
		if (Breakpoint.breakpointsGet.contains(combined)) {
			Debugger.println("Removed breakpoint: " + combined);
			Breakpoint.breakpointsGet.remove(combined);
		} else {
			Debugger.println("Added breakpoint: " + combined);
			Breakpoint.breakpointsGet.add(combined);
		}

		return false;
	}
}
