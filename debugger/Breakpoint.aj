/* Implements the breakpoint feature for the debugger
 */
package debugger;

import java.util.*;
import java.lang.reflect.*;
import org.aspectj.lang.JoinPoint;

privileged aspect Breakpoint {
	static List<String> breakpoints = new ArrayList<String>();

	// Unfortunately this seems to be the only way to do it
	pointcut methodCall(): 
		execution(* **())
		&& !within(debugger.*);
	pointcut methodCall1(Object o):
		execution(* **(..)) && args(o)
		&& !within(debugger.*);
	pointcut methodCall2(Object o1, Object o2):
		execution(* **(..)) && args(o1, o2)
		&& !within(debugger.*);
	pointcut methodCall3(Object o1, Object o2, Object o3):
		execution(* **(..)) && args(o1, o2, o3)
		&& !within(debugger.*);
	pointcut methodCall4(Object o1, Object o2, Object o3, Object o4):
		execution(* **(..)) && args(o1, o2, o3, o4)
		&& !within(debugger.*);
	pointcut methodCall5(Object o1, Object o2, Object o3, Object o4, Object o5):
		execution(* **(..)) && args(o1, o2, o3, o4, o5)
		&& !within(debugger.*);

	static {
		Debugger.commands.add(new BreakpointCommand());
	}

	static class BreakpointCommand implements ICommand {
		public boolean matches(String input) {
			return input.toLowerCase().equals("break");
		}

		public String getUsage() {
			return "break class.method - stop execution when the specified " + 
					"method is called";
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
	
	Object[] breakPoint(Object joinPoint, Object[] args) {
		JoinPoint jp = (JoinPoint) joinPoint;
		String clazz = jp.getSignature().getDeclaringType().getName();
		String name = jp.getSignature().getName();
		String methodName = clazz + "." + name;

		if (breakpoints.contains(methodName)) {
			// Get the parameter types
			Method m = ClassUtils.getMethod(clazz, name);
			Class<?>[] paramTypes = m.getParameterTypes();
			if (paramTypes.length != args.length) {
				System.err.println("Params don't match!?");
				System.exit(1);
			}
			// Do breakpoint prompt
			String fileName = jp.getSourceLocation().getFileName();
			String lineNumber = "" + jp.getSourceLocation().getLine();
			System.out.println("Breakpoint: " + jp.getSignature().toString()
			 	+ "\nat " + fileName + ":" + lineNumber);
			if (args.length == 0) {
				System.out.println("No arguments");
			} else {
				System.out.println("Current arguments: ");
				for (int i = 0; i < args.length; ++i) {
					System.out.println("  Param " + i + 
						" - " + paramTypes[i].getName() +
						" - " + args[i]);
				}
			}
		}

		return args;
	}

	Object around() : methodCall() {
		breakPoint(thisJoinPoint, new Object[] { });
		return proceed();	
	}

	Object around(Object o) : methodCall1(o) {
		Object[] new_args = breakPoint(thisJoinPoint, new Object[] { o });
		return proceed(new_args[0]);
	}

	Object around(Object o1, Object o2) : methodCall2(o1, o2) {
		Object[] new_args = breakPoint(thisJoinPoint, new Object[] { o1, o2 });
		return proceed(new_args[0], new_args[1]);
	}

	Object around(Object o1, Object o2, Object o3) : methodCall3(o1, o2, o3) {
		Object[] new_args = breakPoint(thisJoinPoint, new Object[] { o1, o2, o3 });
		return proceed(new_args[0], new_args[1], new_args[2]);
	}

	Object around(Object o1, Object o2, Object o3, Object o4) : methodCall4(o1, o2, o3, o4) {
		Object[] new_args = breakPoint(thisJoinPoint, new Object[] { o1, o2, o3, o4 });
		return proceed(new_args[0], new_args[1], new_args[2], new_args[3]);
	}

	Object around(Object o1, Object o2, Object o3, Object o4, Object o5) : methodCall5(o1, o2, o3, o4, o5) {
		Object[] new_args = breakPoint(thisJoinPoint, new Object[] { o1, o2, o3, o4, o5 });
		return proceed(new_args[0], new_args[1], new_args[2], new_args[3], new_args[4]);
	}

}
