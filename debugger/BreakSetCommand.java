package debugger;

import java.util.Scanner;

/**
 * Break on class field set. Stops execution when a specified field
 * in a class is about to be set. Control is transferred to the
 * Debugger prompt.
 */
class BreakSetCommand implements ICommand {
	public boolean matches(String input) {
		return input.toLowerCase().equals("breakset");
	}

	public String getHelp() {
		return "break on a field set";
	}

	public String getCommand() {
		return "breakset class.field";
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
		if (Breakpoint.breakpointsSet.contains(combined)) {
			Debugger.println("Removed breakpoint: " + combined);
			Breakpoint.breakpointsSet.remove(combined);
		} else {
			Debugger.println("Added breakpoint: " + combined);
			Breakpoint.breakpointsSet.add(combined);
		}

		return false;
	}
}
