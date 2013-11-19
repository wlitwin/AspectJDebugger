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

		String[] input = line.split("\\s+");
		if (input.length < 2) {
			Debugger.errorln("Requires two arguments - " + getCommand());
			return false;
		}

		if (!Utils.isInteger(input[0])) {
			Debugger.errorln("-> first parameter is not an integer");
			return false;
		}

		int arg_index = Integer.parseInt(input[0]);
		if (arg_index < 0 || arg_index >= breakArgs.length) {
			Debugger.errorln("-> arg number is out of bounds");
			return false;
		}

		// Determine if the second parameter is a String or an integer
		Class<?> argClass = breakArgs[arg_index].getClass();
		Class<?> intClass = Integer.class;
		Class<?> strClass = String.class;
		if (Utils.isInteger(input[1])) {
			if (!intClass.isAssignableFrom(argClass) /*&& 
				!int.class.isAssignableFrom(argClass)*/) {
				Debugger.errorln("Cannot assign an integer to a " + 
						breakArgs[arg_index].getClass().getName());
				return false;
			} else {
				breakArgs[arg_index] = Integer.parseInt(input[1]);
			}
		} else { // Try as a String
			if (!strClass.isAssignableFrom(argClass)) {
				Debugger.errorln("Cannot assign a string to a " +
						breakArgs[arg_index].getClass().getName());
				return false;
			} else {
				breakArgs[arg_index] = input[1];
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
