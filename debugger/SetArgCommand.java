package debugger;

import java.util.Scanner;

/**
 * During a breakpoint the argument values can be changed. Works with all types
 * of breakpoints (method/set/get). Although, it only supports changing of
 * integer and string arguments for now.
 */
class SetArgCommand implements ICommand {
	public boolean matches(String input) {
		return input.toLowerCase().equals("setarg");
	}

	public String getHelp() {
		return "change an argument value, only valid during a breakpoint";
	}

	public String getCommand() {
		return "setarg num val";
	}

	public boolean doWork(Scanner in) {
		String line = in.nextLine().trim();

		if (!Breakpoint.atBreakpointMethod && !Breakpoint.atBreakpointField) {
			Debugger.errorln("This command is only valid during a breakpoint");
			return false;
		}

		Object[] breakArgs = Breakpoint.breakArgs;

		if (breakArgs == null) {
			Debugger.errorln("-> SetArgs - breakArgs is null!");
			return false;
		}

		Scanner sc = new Scanner(line);
		if (!sc.hasNextInt()) {
			Debugger.errorln("-> first parameter is not an integer");
			return false;
		}

		int arg_index = sc.nextInt();
		if (arg_index < 0 || arg_index >= breakArgs.length) {
			Debugger.errorln("-> arg number is out of bounds");
			return false;
		}

		if (!sc.hasNextLine()) {
			Debugger.errorln("Requires two arguments - " + getCommand());
			return false;
		}

		// Determine if the second parameter is a String or an integer
		String input = sc.nextLine().trim();
		Class<?> argClass = breakArgs[arg_index].getClass();
		Class<?> intClass = Integer.class;
		Class<?> strClass = String.class;
		if (Utils.isInteger(input)) {
			if (!intClass.isAssignableFrom(argClass) /*&& 
				!int.class.isAssignableFrom(argClass)*/) {
				Debugger.errorln("Cannot assign an integer to a " + 
						breakArgs[arg_index].getClass().getName());
				return false;
			} else {
				breakArgs[arg_index] = Integer.parseInt(input);
			}
		} else { // Try as a String
			if (!strClass.isAssignableFrom(argClass)) {
				Debugger.errorln("Cannot assign a string to a " +
						breakArgs[arg_index].getClass().getName());
				return false;
			} else {
				// Combine the rest of the array together
				breakArgs[arg_index] = input;
			}
		}

		// Print the new values
		if (Breakpoint.atBreakpointMethod) {
			Breakpoint.printArguments(Breakpoint.breakMethod, breakArgs);
		} else if (Breakpoint.atBreakpointField) {
			Debugger.println("-> New Value: " + breakArgs[arg_index]);
		}

		return false;
	}

}
