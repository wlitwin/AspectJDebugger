package debugger;

import java.util.Scanner;

privileged aspect VariableWatcher {
	// Need a map to see if the variable is being watched
	static {
		Debugger.commands.add(new WatchCommand());
	}

	static class WatchCommand implements ICommand {
		public boolean matches(String input) {
			return input.toLowerCase().equals("watch");
		}
		public String getUsage() {
			return "watch - watch a variable";
		}
		public boolean doWork(Scanner sc) {
			sc.nextLine();
			return false;
		}
	}

	pointcut onSet(Object o, Object value):
		set(* *) && this(o) && 
		args(value) && !within(debugger.*);

	// Grab the new value
	before(Object o, Object value) : onSet(o, value) {
		// Add the variable being set
		System.out.println(thisJoinPoint.getSignature());
		System.out.println(o.getClass() + " setting " + value);
	}
}
