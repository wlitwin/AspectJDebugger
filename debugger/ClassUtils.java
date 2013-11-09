package debugger;

import java.io.*;
import java.util.*;
import java.net.URL;
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
							//System.out.println("Loading! " + name);
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
