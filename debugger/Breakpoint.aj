/* Implements the breakpoint feature for the debugger
 */
package debugger;

import java.util.*;
import java.lang.reflect.*;
import org.aspectj.lang.JoinPoint;
import org.aspectj.lang.annotation.SuppressAjWarnings;

privileged aspect Breakpoint {
	static List<String> breakpoints = new ArrayList<String>();
	static List<String> breakpointsSet = new ArrayList<String>();
	static List<String> breakpointsGet = new ArrayList<String>();

	static boolean atBreakpointMethod = false;
	static boolean atBreakpointField = false;
	static JoinPoint breakJoinPoint;
	static Method breakMethod;
	static Object[] breakArgs;

	// Unfortunately this seems to be the only way to do it
	pointcut methodCall(Object t): 
		execution(* **()) && target(t)
		&& !within(debugger.*);
	pointcut methodCall1(Object t, Object o):
		execution(* **(..)) && args(o) && target(t)
		&& !within(debugger.*);
	pointcut methodCall2(Object t, Object o1, Object o2):
		execution(* **(..)) && args(o1, o2) && target(t)
		&& !within(debugger.*);
	pointcut methodCall3(Object t, Object o1, Object o2, Object o3):
		execution(* **(..)) && args(o1, o2, o3) && target(t)
		&& !within(debugger.*);
	pointcut methodCall4(Object t, Object o1, Object o2, Object o3, Object o4):
		execution(* **(..)) && args(o1, o2, o3, o4) && target(t)
		&& !within(debugger.*);
	pointcut methodCall5(Object t, Object o1, Object o2, Object o3, Object o4, Object o5):
		execution(* **(..)) && args(o1, o2, o3, o4, o5) && target(t)
		&& !within(debugger.*);

	pointcut fieldGet(Object t, Object a) :
		get(* *)
		&& args(a) && target(t) 
		&& !within(debugger.*);

	pointcut fieldGet2(Object t) :
		get(* *)
		&& target(t) 
		&& !within(debugger.*);

	pointcut fieldSet(Object t, Object a) :
		set(* *)
		&& args(a) && target(t) 
		&& !within(debugger.*);

	static {
		Debugger.commands.add(new BreakpointCommand());
		Debugger.commands.add(new SetArgCommand());
		Debugger.commands.add(new BreakGetCommand());
		Debugger.commands.add(new BreakSetCommand());
	}

	static class BreakpointCommand implements ICommand {
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
				System.out.println("Invalid, usage is - break class.method");	
				return false;
			}

			String clazz = classAndMethod[0];
			String method = classAndMethod[1];

			if (!ClassUtils.isValidClass(clazz)) {
				System.out.println("Couldn't find class: " + clazz);
				return false;
			}

			if (!ClassUtils.isValidMethod(clazz, method)) {
				System.out.println("Couldn't find method: " + method);
				return false;
			}

			String combined = clazz + "." + method;
			if (breakpoints.contains(combined)) {
				System.out.println("Removed breakpoint: " + combined);	
				breakpoints.remove(combined);
			} else {
				System.out.println("Added breakpoint: " + combined);
				breakpoints.add(combined);
			}
			
			return false;
		}
	}

	static class BreakSetCommand implements ICommand {
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
				System.out.println("Invalid, usage is - " + getCommand());
				return false;
			}

			String clazz = classAndField[0];
			String field = classAndField[1];

			if (!ClassUtils.isValidClass(clazz)) {
				System.out.println("Couldn't find class: " + clazz);
				return false;
			}

			if (!ClassUtils.isValidField(clazz, field)) {
				System.out.println("Couldn't find field: " + field);
				return false;
			}

			String combined = clazz + "." + field;
			if (breakpointsSet.contains(combined)) {
				System.out.println("Removed breakpoint: " + combined);
				breakpointsSet.remove(combined);
			} else {
				System.out.println("Added breakpoint: " + combined);
				breakpointsSet.add(combined);
			}

			return false;
		}
	}

	static class BreakGetCommand implements ICommand {
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
				System.out.println("Invalid, usage is - " + getCommand());
				return false;
			}

			String clazz = classAndField[0];
			String field = classAndField[1];

			if (!ClassUtils.isValidClass(clazz)) {
				System.out.println("Couldn't find class: " + clazz);
				return false;
			}

			if (!ClassUtils.isValidField(clazz, field)) {
				System.out.println("Couldn't find field: " + field);
				return false;
			}

			String combined = clazz + "." + field;
			if (breakpointsGet.contains(combined)) {
				System.out.println("Removed breakpoint: " + combined);
				breakpointsGet.remove(combined);
			} else {
				System.out.println("Added breakpoint: " + combined);
				breakpointsGet.add(combined);
			}

			return false;
		}
	}

	static class SetArgCommand implements ICommand {
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

			if (!atBreakpointMethod && !atBreakpointField) {
				System.out.println("This command is only valid during a breakpoint");
				return false;
			}

			if (breakArgs == null) {
				System.out.println("-> SetArgs - breakArgs is null!");
				return false;
			}

			String[] input = line.split("\\s+");
			if (input.length < 2) {
				System.out.println("Requires two arguments - " + getCommand());
				return false;
			}

			if (!Utils.isInteger(input[0])) {
				System.out.println("-> first parameter is not an integer");
				return false;
			}

			int arg_index = Integer.parseInt(input[0]);
			if (arg_index < 0 || arg_index >= breakArgs.length) {
				System.out.println("-> arg number is out of bounds");
				return false;
			}

			// Determine if the second parameter is a String or an integer
			Class<?> argClass = breakArgs[arg_index].getClass();
			Class<?> intClass = Integer.class;
			Class<?> strClass = String.class;
			if (Utils.isInteger(input[1])) {
				if (!intClass.isAssignableFrom(argClass)) {
					System.out.println("Cannot assign an integer to a " + 
						breakArgs[arg_index].getClass().getName());
					return false;
				} else {
					breakArgs[arg_index] = Integer.parseInt(input[1]);
				}
			} else { // Try as a String
				if (!strClass.isAssignableFrom(argClass)) {
					System.out.println("Cannot assign a string to a " +
						breakArgs[arg_index].getClass().getName());
					return false;
				} else {
					breakArgs[arg_index] = input[1];
				}
			}

			// Print the new values
			if (atBreakpointMethod) {
				printArguments(breakMethod, breakArgs);
			} else if (atBreakpointField) {
				System.out.println("-> New Value: " + breakArgs[arg_index]);
			}

			return false;
		}

	}

	static void printArguments(Method m, Object[] args) {
		if (!atBreakpointMethod) {
			System.out.println("-> printArguments() - invalid call");
			return;
		}

		if (args.length == 0) {
			System.out.println("-> No arguments");
		} else {
			Class<?>[] paramTypes = m.getParameterTypes();
			if (paramTypes.length != args.length) {
				System.err.println("Params don't match!?");
				System.exit(1);
			}
			System.out.println("-> Current arguments: ");
			for (int i = 0; i < args.length; ++i) {
				System.out.println("->  Param " + i + 
						" - " + paramTypes[i].getName() +
						" - " + args[i]);
			}
		}
	}

	static void printLocation(JoinPoint jp, Object target) {
		String fileName = jp.getSourceLocation().getFileName();
		String lineNumber = "" + jp.getSourceLocation().getLine();
		int hashCode = System.identityHashCode(target);
		System.out.println("-> [" + hashCode + "] Breakpoint: "
				+ jp.getSignature().toString()
				+ "\n-> at " + fileName + ":" + lineNumber);
	}

	static String[] classNameAndCombined(JoinPoint jp) {
		String clazz = jp.getSignature().getDeclaringType().getName();
		String name = jp.getSignature().getName();
		String combined = clazz + "." + name;

		return new String[] { clazz, name, combined };
	}

	Object[] breakGet(Object target, Object joinPoint) {
		JoinPoint jp = (JoinPoint) joinPoint;
		breakJoinPoint = jp;
		String[] all = classNameAndCombined(jp);
		
		if (breakpointsGet.contains(all[2])) {
			System.out.println("-> Get");
			printLocation(jp, target);

			Field f = ClassUtils.getField(all[0], all[1]);	
			f.setAccessible(true);
			Object o = null;
			try {
				o = f.get(target);
				System.out.println("-> Cur Value: " + o);
			} catch (IllegalAccessException iae) {
				System.out.println("-> Cannot access field value");
			}

			atBreakpointMethod = false;
			atBreakpointField = true;

			breakArgs = new Object[] { o };
			Debugger.prompt();

			o = breakArgs[0];
			breakArgs = null;
			atBreakpointField = false;

			return new Object[] { true, o };
		}

		return new Object[] { false, null };
	}

	Object breakSet(Object target, Object joinPoint, Object arg) {
		JoinPoint jp = (JoinPoint) joinPoint;
		breakJoinPoint = jp;
		String[] all = classNameAndCombined(jp);
		
		if (breakpointsSet.contains(all[2])) {
			System.out.println("-> Set");
			printLocation(jp, target);

			Field f = ClassUtils.getField(all[0], all[1]);	
			f.setAccessible(true);
			try {
				Object o = f.get(target);
				System.out.println("-> Cur Value: " + o);
				System.out.println("-> New Value: " + arg);
			} catch (IllegalAccessException iae) {
				System.out.println("-> Cannot access field value");
			}

			atBreakpointMethod = false;
			atBreakpointField = true;

			breakArgs = new Object[] { arg };
			Debugger.prompt();

			arg = breakArgs[0];
			breakArgs = null;
			atBreakpointField = false;

		}

		return arg;
	}

	Object[] breakPoint(Object target, Object joinPoint, Object[] args) {
		JoinPoint jp = (JoinPoint) joinPoint;
		breakJoinPoint = jp;
		String[] all = classNameAndCombined(jp);

		if (breakpoints.contains(all[2])) {
			// Get the parameter types
			breakMethod = ClassUtils.getMethod(all[0], all[1]);
			printLocation(jp, target);
			printArguments(breakMethod, args);

			// Enter the debugger prompt
			atBreakpointMethod = true;
			atBreakpointField = false;

			breakArgs = args;
			Debugger.prompt();

			breakArgs = null;
			atBreakpointMethod = false;
		}

		return args;
	}

	@SuppressAjWarnings({"adviceDidNotMatch"})
	Object around(Object t) : fieldGet2(t) {
		Object[] ret = breakGet(t, thisJoinPoint);
		if (ret[0].equals(true)) {
			return ret[1];
		}

		return proceed(t);
	}

	@SuppressAjWarnings({"adviceDidNotMatch"})
	Object around(Object t, Object a) : fieldSet(t, a) {
		Object new_arg = breakSet(t, thisJoinPoint, a);
		return proceed(t, new_arg);
	}

	@SuppressAjWarnings({"adviceDidNotMatch"})
	Object around(Object t) : methodCall(t) {
		breakPoint(t, thisJoinPoint, new Object[] { });
		return proceed(t);	
	}

	@SuppressAjWarnings({"adviceDidNotMatch"})
	Object around(Object t, Object o) : methodCall1(t, o) {
		Object[] new_args = breakPoint(t, thisJoinPoint, new Object[] { o });
		return proceed(t, new_args[0]);
	}

	@SuppressAjWarnings({"adviceDidNotMatch"})
	Object around(Object t, Object o1, Object o2) : methodCall2(t, o1, o2) {
		Object[] new_args = breakPoint(t, thisJoinPoint, new Object[] { o1, o2 });
		return proceed(t, new_args[0], new_args[1]);
	}

	@SuppressAjWarnings({"adviceDidNotMatch"})
	Object around(Object t, Object o1, Object o2, Object o3) : methodCall3(t, o1, o2, o3) {
		Object[] new_args = breakPoint(t, thisJoinPoint, new Object[] { o1, o2, o3 });
		return proceed(t, new_args[0], new_args[1], new_args[2]);
	}

	@SuppressAjWarnings({"adviceDidNotMatch"})
	Object around(Object t, Object o1, Object o2, Object o3, Object o4) : 
	methodCall4(t, o1, o2, o3, o4) {
		Object[] new_args = breakPoint(t, thisJoinPoint, new Object[] { o1, o2, o3, o4 });
		return proceed(t, new_args[0], new_args[1], new_args[2], new_args[3]);
	}

	@SuppressAjWarnings({"adviceDidNotMatch"})
	Object around(Object t, Object o1, Object o2, Object o3, Object o4, Object o5) : 
	methodCall5(t, o1, o2, o3, o4, o5) {
		Object[] new_args = breakPoint(t, thisJoinPoint, new Object[] { o1, o2, o3, o4, o5 });
		return proceed(t, new_args[0], new_args[1], new_args[2], new_args[3], new_args[4]);
	}

}
