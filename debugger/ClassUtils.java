package debugger;

import java.lang.reflect.*;

public class ClassUtils {
	public static boolean isValidClass(String name) {
		try {	
			Class<?> c = Class.forName(name);
			return true;
		} catch (ClassNotFoundException cnfe) {
			return false;
		}
	}

	public static Class<?> getClass(String name) {
		try {
			return Class.forName(name);
		} catch (ClassNotFoundException cnfe) {
			return null;
		}
	}

	public static boolean isValidMethod(String clazz, String name) {
		if (!isValidClass(clazz)) {
			return false;
		}
		
		Class<?> c = getClass(clazz);
		Method[] methods = c.getDeclaredMethods();
		for (Method m : methods) {
			if (m.getName().equals(name)) {
				return true;
			}
		}

		return false;
	}

	public static boolean isValidField(String clazz, String name) {
		if (!isValidClass(clazz)) {
			return false;
		}

		Class<?> c = getClass(clazz);
		Field[] fields = c.getDeclaredFields();
		for (Field f : fields) {
			if (f.getName().equals(name)) {
				return true;
			}
		}

		return false;
	}
}
