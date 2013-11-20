package debugger;

import java.util.*;
import java.lang.ref.*;

import org.aspectj.lang.annotation.SuppressAjWarnings;

public aspect Instances {
	static {
		Debugger.commands.add(new InstancesCommand());
	}

	static Map<Class<?>, Set<WeakReference>> instances = 
		new HashMap<Class<?>, Set<WeakReference>>();

	pointcut onNew(Object o) :
		execution(**.new(..)) && this(o) 
		&& !within(debugger.*);

	static void cleanupReferences() {
		for (Set<WeakReference> swr : instances.values()) {
			Iterator<WeakReference> it = swr.iterator();
			while (it.hasNext()) {
				WeakReference wr = it.next();
				if (wr.get() == null) {
					it.remove();
				}
			}
		}
	}

	@SuppressAjWarnings({"adviceDidNotMatch"})
	before(Object o) : onNew(o) {
		Set<WeakReference> hs = null;

		cleanupReferences();

		if (instances.containsKey(o.getClass())) {
			hs = instances.get(o.getClass());
			// Make sure we don't have this object already
			for (WeakReference wr : hs) {
				if (wr.get() == o) {
					return;
				}
			}
		} else {
			hs = new HashSet<WeakReference>();
			instances.put(o.getClass(), hs);
		}

		hs.add(new WeakReference(o));
	}

	static class InstancesCommand implements ICommand {
		public boolean matches(String input) {
			return input.toLowerCase().equals("instances");
		}

		public String getHelp() {
			return "find all instances of a class";
		}

		public String getCommand() {
			return "instances class";
		}

		public boolean doWork(Scanner in) {
			String line = in.nextLine().trim();

			if (line.isEmpty()) {
				// List all loaded instances
				cleanupReferences();
				boolean atLeastOne = false;
				for (Class<?> c : instances.keySet()) {
					Set<WeakReference> swr = instances.get(c);
					if (!swr.isEmpty()) {
						atLeastOne = true;
						Debugger.println(c);
					}
				}

				if (!atLeastOne) {
					Debugger.println("-> No saved instances");
				}
				return false;
			}

			if (!ClassUtils.isValidClass(line)) {
				Debugger.errorln("-> " + line + " is not a valid class");
				return false;
			}

			Class<?> c = ClassUtils.getClass(line);
			if (!instances.containsKey(c)) {
				Debugger.errorln("-> There are no instances of " + c);
				return false;
			}

			Set<WeakReference> s = instances.get(c);
			Iterator<WeakReference> it = s.iterator();

			while (it.hasNext()) {
				WeakReference w = it.next();
				Object o = w.get();
				if (o != null) {
					int hashCode = System.identityHashCode(o);
					Debugger.println("-> [" + hashCode + "]");
				} else {
					// Cleanup this reference
					it.remove();
				}
			}

			return false;
		}
	}
}
