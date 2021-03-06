package debugger;

import java.io.*;
import java.util.*;
import java.net.URL;
import java.util.zip.*;
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
	 * Similar to getMethod(String, String), except it will parse
	 * the fully qualified name for you.
	 *
	 * @param fullyQualified The fully qualified class.method name
	 *
	 * @throws Exception If the class or method doesn't exist
	 *
	 * @return A method object representing the requested method
	 */
	public static Method getMethod(String fullyQualified) throws Exception {
		int index = fullyQualified.lastIndexOf('.');
		if (index == -1) {
			throw new Exception("Invalid qualified name");
		}

		String clazz = fullyQualified.substring(0, index);
		String method = fullyQualified.substring(index + 1);
		if (!isValidClass(clazz)) {
			throw new Exception("No such class: " + clazz);
		}

		if (!isValidMethod(clazz, method)) {
			throw new Exception("No such method: " + method + " for class: " + clazz);
		}

		return getMethod(clazz, method);
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
		} catch (Exception e) {
			System.out.println(e.getMessage());
			return false;
		}

		return true;
	}

	/**
	 * Loads and initializes all classes from a jar file.
	 *
	 * @param jar The path to the jar file
	 *
	 * @return True if successfully loaded all classes, false otherwise
	 */
	public static boolean loadAllClassesInJar(String jar) {
		try {
			ClassLoader cl = ClassLoader.getSystemClassLoader();
			ZipInputStream zip = new ZipInputStream(new FileInputStream(jar));
			for (ZipEntry ze = zip.getNextEntry(); ze != null; ze = zip.getNextEntry()) {
				if (!ze.isDirectory() && ze.getName().endsWith(".class")) {
					String path = ze.getName();
					path = path.substring(0, path.length() - 6);
					path = path.replace('/', '.');
					Class.forName(path, true, cl);
				}
			
			}
		} catch (Exception e) {
			System.out.println(e.getMessage());
			return false;
		}

		return true;
	}

}
