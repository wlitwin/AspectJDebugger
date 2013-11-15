package debugger;

import java.io.*;
import java.util.*;
import java.net.URL;
import java.lang.reflect.*;

/**
 * Holds many helper methods for getting various information
 * using Java's reflection capabilities.
 */
public class ClassUtils {
	/**
	 * Figures out if a class with the given name exists.
	 * The name should be the fully qualified name.
	 *
	 * @param name The fully qualified class name
	 *
	 * @return True if it exists, false otherwise
	 */
	public static boolean isValidClass(String name) {
		try {	
			Class<?> c = Class.forName(name);
			return true;
		} catch (ClassNotFoundException cnfe) {
			return false;
		}
	}

	/**
	 * Gets the Class<?> object representing the class
	 * with the given name. The name should be the
	 * fully qualified name.
	 *
	 * @param name The fully qualified class name
	 *
	 * @return Class<?> if a class with that name exists 
	 * and null otherwise
	 */
	public static Class<?> getClass(String name) {
		try {
			return Class.forName(name);
		} catch (ClassNotFoundException cnfe) {
			return null;
		}
	}

	/**
	 * Determines if a class has a method with the provided name.
	 * The class name should be the fully qualified name.
	 *
	 * @param clazz The fully qualified class name
	 *
	 * @param name The method name
	 *
	 * @return True if the class exists and has a method with that
	 * name and false otherwise
	 */
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

	/**
	 * Gets a Method object for the method of the class.
	 * The class name should be the fully qualified class
	 * name.
	 *
	 * @param clazz The fully qualified class name
	 *
	 * @param name The method name
	 *
	 * @return A Method object if the class exists and has
	 * a method with the given name, and null otherwise
	 */
	public static Method getMethod(String clazz, String name) {
		if (!isValidClass(clazz) || !isValidMethod(clazz, name)) {
			return null;
		}

		Class<?> c = getClass(clazz);
		Method[] methods = c.getDeclaredMethods();
		for (Method m : methods) {
			if (m.getName().equals(name)) {
				return m;
			}
		}

		return null;
	}

	/**
	 * Determines if a class has a field with the specified name. 
	 *
	 * @param clazz The fully qualified class name
	 *
	 * @param name The name of the field
	 *
	 * @return True if the class exists and has a field with the
	 * given name, false otherwise
	 */
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

	/**
	 * Gets a Field object representing the classes field.
	 *
	 * @param clazz The fully qualified class name
	 *
	 * @param name The name of the field
	 *
	 * @return A Field object if the class exists and has
	 * a field with the provided name, and null otherwise
	 */
	public static Field getField(String clazz, String name) {
		if (!isValidClass(clazz) || !isValidField(clazz, name)) {
			return null;
		}

		Class<?> c = getClass(clazz);
		Field[] fields = c.getDeclaredFields();
		for (Field f : fields) {
			if (f.getName().equals(name)) {
				return f;
			}
		}

		return null;
	}

	/**
	 * Loads and initializes all classes in a package. This
	 * causes all static initializers in those classes to be
	 * run, normally this would happen when a class is first
	 * used.
	 *
	 * @param pkg The fully qualified package name
	 *
	 * @return True if everything worked, false if the package
	 * doesn't exist or an exception occurred during loading
	 */
	public static boolean loadAllClassesInPackage(String pkg) {
		try {
			// TODO make more robust
			ClassLoader cl = ClassLoader.getSystemClassLoader();
			Enumeration<URL> urls = cl.getResources(pkg);
			//System.out.println("Grabbed Resources");
			for (; urls.hasMoreElements();) {
				File file = new File(urls.nextElement().getFile());
				if (file.isDirectory()) {
					File[] classes = file.listFiles();	
					for (File f : classes) {
						if (f.getName().endsWith(".class")) {
							String name = f.getName();
							name = name.substring(0, name.indexOf("."));
							name = pkg + "." + name;
							// Initialize this class
							Class.forName(name, true, cl);
						}
					}
				}
			}
		} catch (ClassNotFoundException e) {
			return false;
		} catch (IOException e) {
			return false;
		}

		return true;
	}

}
