/* Implements the breakpoint feature for the debugger
 */
package debugger;

import java.util.*;
import java.lang.reflect.*;
import org.aspectj.lang.JoinPoint;
import org.aspectj.lang.annotation.SuppressAjWarnings;

/**
 * Aspect handling the breakpoint functionality. It has around advice for method calls of up
 * to 5 arguments as well as field sets and gets.
 */
public aspect Breakpoint {
	static List<String> breakpoints = new ArrayList<String>();
	static List<String> breakpointsSet = new ArrayList<String>();
	static List<String> breakpointsGet = new ArrayList<String>();
	static Map<String, CondBreakpoint> condBreakpoints = new HashMap<String, CondBreakpoint>();

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

	pointcut fieldGet(Object t) :
		get(* *)
		&& target(t) 
		&& !within(debugger.*);

	pointcut fieldSet(Object t, Object a) :
		set(* *)
		&& args(a) && target(t) 
		&& !within(debugger.*);

	// Add the commands this Aspect offers
	static {
		Debugger.commands.add(new BreakpointCommand());
		Debugger.commands.add(new SetArgCommand());
		Debugger.commands.add(new BreakGetCommand());
		Debugger.commands.add(new BreakSetCommand());
		Debugger.commands.add(new CondBreakpoint.BreakCondCommand());
		Debugger.commands.add(new CondBreakpoint.BreakCondRemoveCommand());
	}


	/**
	 * Helper method to print the arguments of a method. Should only be called
	 * during a method breakpoint.
	 *
	 * @param m The method object
	 *
	 * @param args The arguments to this method object
	 */
	static void printArguments(Method m, Object[] args) {
		if (!atBreakpointMethod) {
			Debugger.errorln("-> printArguments() - invalid call");
			return;
		}

		if (args.length == 0) {
			Debugger.println("-> No arguments");
		} else {
			Class<?>[] paramTypes = m.getParameterTypes();
			if (paramTypes.length != args.length) {
				System.err.println("Params don't match!?");
				System.exit(1);
			}
			Debugger.println("-> Current arguments: ");
			for (int i = 0; i < args.length; ++i) {
				Debugger.println("->  Param " + i + 
						" - " + paramTypes[i].getName() +
						" - " + args[i]);
			}
		}
	}

	/**
	 * Prints where the breakpoint happend in the source code.
	 *
	 * @param jp The joinpoint
	 *
	 * @param target The object that is being intercepted
	 */
	static void printLocation(JoinPoint jp, Object target) {
		String fileName = jp.getSourceLocation().getFileName();
		String lineNumber = "" + jp.getSourceLocation().getLine();
		int hashCode = System.identityHashCode(target);
		Debugger.println("-> [" + hashCode + "] Breakpoint: "
				+ jp.getSignature().toString()
				+ "\n-> at " + fileName + ":" + lineNumber);
	}

	/**
	 * Extracts the class name and field or method name from a JoinPoint.
	 * 
	 * @param jp The join point
	 *
	 * @return A three element array where the first element is the class
	 * name, the second is the field/method name and the third is the
	 * class and field/method named combined with a '.'
	 */
	static String[] classNameAndCombined(JoinPoint jp) {
		String clazz = jp.getSignature().getDeclaringType().getName();
		String name = jp.getSignature().getName();
		String combined = clazz + "." + name;

		return new String[] { clazz, name, combined };
	}

	/**
	 * Called when the fieldGet around advice happens. This will determine
	 * if the debugger prompt should be invoked. It checks the breakpointsGet
	 * list to see if the string class.field is in it.
	 *
	 * @param target The object being called
	 *
	 * @param joinPoint Information about the field and object
	 *
	 * @return The modified parameters to this method
	 */
	private Object[] breakGet(Object target, Object joinPoint) {
		JoinPoint jp = (JoinPoint) joinPoint;
		breakJoinPoint = jp;
		String[] all = classNameAndCombined(jp);
		
		if (breakpointsGet.contains(all[2])) {
			atBreakpointMethod = false;
			atBreakpointField = true;

			Debugger.println("-> Get");
			printLocation(jp, target);

			Field f = ClassUtils.getField(all[0], all[1]);	
			f.setAccessible(true);
			Object o = null;
			try {
				o = f.get(target);
				Debugger.println("-> Cur Value: " + o);
			} catch (IllegalAccessException iae) {
				Debugger.errorln("-> Cannot access field value");
			}

			breakArgs = new Object[] { o };
			Debugger.prompt();

			o = breakArgs[0];
			breakArgs = null;
			atBreakpointField = false;

			return new Object[] { true, o };
		}

		return new Object[] { false, null };
	}

	/**
	 * Called when the fieldSet around advice happens. This will determine
	 * if the debugger prompt should be invoked. It checks the breakpointsSet
	 * list to see if the string class.field is in it.
	 *
	 * @param target The object being called
	 *
	 * @param joinPoint Information about the field and object
	 *
	 * @return A possibly different value to set the field to
	 */
	private Object breakSet(Object target, Object joinPoint, Object arg) {
		JoinPoint jp = (JoinPoint) joinPoint;
		breakJoinPoint = jp;
		String[] all = classNameAndCombined(jp);
		
		if (breakpointsSet.contains(all[2])) {
			atBreakpointMethod = false;
			atBreakpointField = true;

			Debugger.println("-> Set");
			printLocation(jp, target);

			Field f = ClassUtils.getField(all[0], all[1]);	
			f.setAccessible(true);
			try {
				Object o = f.get(target);
				Debugger.println("-> Cur Value: " + o);
				Debugger.println("-> New Value: " + arg);
			} catch (IllegalAccessException iae) {
				Debugger.errorln("-> Cannot access field value");
			}

			breakArgs = new Object[] { arg };
			Debugger.prompt();

			arg = breakArgs[0];
			breakArgs = null;
			atBreakpointField = false;

		}

		return arg;
	}

	/**
	 * Called when the methodCall around advice is invoked. If the class and method
	 * are in the breakpoints list then the debugger prompt will be invoked.
	 *
	 * @param target The object being called
	 *
	 * @param joinPoint Context about the call
	 *
	 * @param args The arguments to the method
	 *
	 * @return Possibly changed arguments to the method
	 */
	private Object[] breakPoint(Object target, Object joinPoint, Object[] args) {
		JoinPoint jp = (JoinPoint) joinPoint;
		breakJoinPoint = jp;
		String[] all = classNameAndCombined(jp);

		boolean condBreakpoint = false;
		if (condBreakpoints.containsKey(all[2])) {
			CondBreakpoint cb = condBreakpoints.get(all[2]);
			condBreakpoint = cb.evaluate(args);
		}

		if (breakpoints.contains(all[2]) || condBreakpoint) {
			// Enter the debugger prompt
			atBreakpointMethod = true;
			atBreakpointField = false;

			// Get the parameter types
			breakMethod = ClassUtils.getMethod(all[0], all[1]);
			printLocation(jp, target);
			printArguments(breakMethod, args);

			breakArgs = args;
			Debugger.prompt();

			breakArgs = null;
			atBreakpointMethod = false;
		}

		return args;
	}

	// Handles fieldGet
	@SuppressAjWarnings({"adviceDidNotMatch"})
	Object around(Object t) : fieldGet(t) {
		Object[] ret = breakGet(t, thisJoinPoint);
		if (ret[0].equals(true)) {
			return ret[1];
		}

		return proceed(t);
	}

	// Handles fieldSet
	@SuppressAjWarnings({"adviceDidNotMatch"})
	Object around(Object t, Object a) : fieldSet(t, a) {
		Object new_arg = breakSet(t, thisJoinPoint, a);
		return proceed(t, new_arg);
	}

	//=========================================================================
	// The around() blocks below handle method calls of varying arities
	//=========================================================================
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
