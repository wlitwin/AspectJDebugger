package debugger;

import java.util.*;

privileged aspect VariableWatcher {
	// Need a map to see if the variable is being watched
	static {
		Debugger.commands.add(new WatchCommand());
		Debugger.commands.add(new UnWatchCommand());
	}

	static List<String> watchedVariables = new ArrayList<String>();

	static class UnWatchCommand implements ICommand {
		public boolean matches(String input) {
			return input.toLowerCase().equals("unwatch");
		}

		public String getHelp() {
			return "stop watching a variable";
		}

		public String getCommand() {
			return "unwatch class.field";
		}

		public boolean doWork(Scanner sc) {
			String line = sc.nextLine().trim();
			if (watchedVariables.contains(line)) {
				watchedVariables.remove(line);
				System.out.println("Unwatching: " + line);
			} else {
				System.out.println("Not watching: " + line);
			}

			return false;
		}
	}

	static class WatchCommand implements ICommand {
		public boolean matches(String input) {
			return input.toLowerCase().equals("watch");
		}

		public String getHelp() {
			return "watch a variable";
		}

		public String getCommand() {
			return "watch class.field";
		}

		public boolean doWork(Scanner sc) {
			String line = sc.nextLine();
			String[] classAndVar = line.trim().split("\\.");
			if (classAndVar.length < 2) {
				System.out.println("Invalid, usage is - watch class.field");
				return false;
			}

			// Try to find the class
			if (!ClassUtils.isValidClass(classAndVar[0])) {
				System.out.println("Couldn't find class: " + classAndVar[0]);
				return false;
			}

			// Try to find the field
			if (!ClassUtils.isValidField(classAndVar[0], classAndVar[1])) {
				System.out.println("Couldn't find field: " + classAndVar[1]);
				return false;
			}

			// Add it to the watched list
			String combined = classAndVar[0] + "." + classAndVar[1];
			if (watchedVariables.contains(combined)) {
				System.out.println("Already watching: " + combined);
			} else {
				System.out.println("Watching: " + combined);	
				watchedVariables.add(combined);
			}

			return false;
		}
	}

	pointcut onSet(Object o, Object value):
		set(* *) && this(o) && 
		args(value) && !within(debugger.*);

	// Grab the new value
	before(Object o, Object value) : onSet(o, value) {
		// Add the variable being set
		String fullName = thisJoinPoint.getSignature().toString();
		String varAndClass = fullName.split(" ")[1];
		if (watchedVariables.contains(varAndClass)) {
			System.out.println("-> " + fullName + " = " + value);	
		}
	}
}
