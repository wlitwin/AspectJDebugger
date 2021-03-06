package debugger;

import java.util.*;
import java.lang.reflect.*;
import org.aspectj.lang.JoinPoint;
import org.aspectj.lang.annotation.SuppressAjWarnings;

privileged aspect Replacer {
	static {
		Debugger.commands.add(new ReplaceCommand());
		Debugger.commands.add(new ReplaceRemoveCommand());
	}

	static Map<String, Method> replacements = new HashMap<String, Method>();

	static class ReplaceCommand implements ICommand {
		public boolean matches(String input) {
			return input.toLowerCase().equals("replace");
		}

		public String getHelp() {
			return "replace a method on an object with a call to a static method";
		}

		public String getCommand() {
			return "replace source.method dest.method";
		}

		public boolean doWork(Scanner sc) {
			String line = sc.nextLine().trim();

			String[] args = line.split("\\s+");
			if (args.length != 2) {
				Debugger.errorln("Usage: " + getCommand());
				return false;
			}

			// Now we need to compare the methods to make sure
			// they're compatible
			try {
				Method m1 = ClassUtils.getMethod(args[0]);
				Method m2 = ClassUtils.getMethod(args[1]);

				// The second method needs all the same arguments as
				// the first method, except it also takes an extra
				// first parameter which is the target object being
				// called. The second method must also be static.
				if (!Modifier.isStatic(m2.getModifiers())) {
					Debugger.errorln("The second method must be static");
					return false;
				}

				// Check their parameters
				Class<?>[] t1 = m1.getParameterTypes();
				Class<?>[] t2 = m2.getParameterTypes();

				// M2 must have an additional parameter
				if (t2.length != t1.length + 1) {
					Debugger.errorln("The second method must have an additional parameter");
					return false;
				}

				// M2's first parameter must be of type Object
				int index = args[0].lastIndexOf('.');
				String className = args[0].substring(0, index);
				Class<?> c1 = ClassUtils.getClass(className);
				if (t2[0] != Object.class && t2[0] != c1) {
					Debugger.errorln("The second method must have a first parameter of type Object");
					return false;
				}

				// Check the rest of the parameters
				for (int i = 0; i < t1.length; ++i) {
					if (!t1[i].isAssignableFrom(t2[i + 1])) {
						Debugger.errorln("Parameter mismatch [" + i + "]: m1 - " 
							+ t1[i] + " m2 - " + t2[i + 1]);
						return false;
					}
				}

				// Now we can save this replacement
				if (replacements.containsKey(args[0])) {
					Debugger.println("Updating: " + args[0] + " to point to " + args[1]);
				} else {
					Debugger.println("Replacing: " + args[0] + " with " + args[1]);
				}

				replacements.put(args[0], m2);
			} catch (Exception e) {
				Debugger.errorln(e.getMessage());
			}

			return false;
		}
	}

	static class ReplaceRemoveCommand implements ICommand {
		public boolean matches(String input) {
			return input.toLowerCase().equals("replaceremove");
		}

		public String getHelp() {
			return "remove a current replacement";
		}

		public String getCommand() {
			return "replaceremove class.method";
		}

		public boolean doWork(Scanner in) {
			String line = in.nextLine().trim();

			if (replacements.containsKey(line)) {
				Debugger.println("Removed replacement for " + line);
				replacements.remove(line);
			} else {
				Debugger.println("No replacement for " + line);
			}

			return false;
		}
	}

	// We're going to use the same trick as Breakpoint.aj
	pointcut methodCall(Object t): 
		execution(* **(..)) && target(t)
		&& !within(debugger..*);

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

	static boolean shouldReplace(JoinPoint jp) {
		return replacements.containsKey(classNameAndCombined(jp)[2]);
	}

	static Object doReplace(JoinPoint jp, Object target) {
		Object[] jpArgs = jp.getArgs();
		Object[] args = new Object[jpArgs.length + 1];
		args[0] = target;
		for (int i = 0; i < jpArgs.length; ++i) {
			args[i+1] = jpArgs[i];	
		}

		String combined = classNameAndCombined(jp)[2];
		if (!replacements.containsKey(combined)) {
			throw new RuntimeException("Replacement: Shouldn't happen!");
		}

		Method m = replacements.get(combined);

		try {
			Object o = m.invoke(null, args);
			return o;
		} catch (IllegalAccessException iae) {
			Debugger.errorln("Illegal access exception");
		} catch (IllegalArgumentException iae) {
			Debugger.errorln("Illegal argument exception");
		} catch (InvocationTargetException ite) {
			Debugger.errorln(ite.getCause().getMessage());
			Debugger.errorln("Invocation target exception");
		} catch (NullPointerException npe) {
			Debugger.errorln("Null pointer exception");
		} catch (ExceptionInInitializerError eiie) {
			Debugger.errorln("ExceptionInInitializerError");
		} catch (Exception e) {
			if (e.getMessage() != null) {
				Debugger.errorln(e.getMessage());
			} else {
				Debugger.errorln("Replacer: Unknown error");
			}
		}

		return null;
	}

	@SuppressAjWarnings({"adviceDidNotMatch"})
	Object around(Object t) : methodCall(t) {
		if (shouldReplace(thisJoinPoint)) {
			return doReplace(thisJoinPoint, t);
		} else {
			return proceed(t);
		}
	}
}
