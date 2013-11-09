package debugger;

import java.util.*;
import java.lang.reflect.*;

public class Debugger {
	static final Scanner in = new Scanner(System.in);
	static final List<ICommand> commands = new LinkedList<ICommand>();
	static {
		commands.add(new HelpCommand());
		commands.add(new QuitCommand());
		commands.add(new ListFieldsCommand());
		commands.add(new ListMethodsCommand());
		commands.add(new GoCommand());

		// Force all other classes to be initialized
		if (!ClassUtils.loadAllClassesInPackage("debugger")) {
			System.err.println("Failed to load debugger!");
			System.exit(1);
		}
	}

	// Interesting!
	{
		System.out.println("CODE!");
	}

	public static Scanner getScanner() {
		return in;
	}

	public static void prompt() {
		try {
			System.out.println("Entering prompt!");
			boolean finished = false;
			while (!finished) {
				System.out.print("> ");
				System.out.flush();
				String line = in.next();

				boolean foundCommand = false;
				for (ICommand cmd : commands) {
					if (cmd.matches(line)) {
						foundCommand = true;
						finished = cmd.doWork(in);	
						break;
					}
				}

				if (!foundCommand) {
					in.nextLine(); // Clear what the entered
					System.out.println("Invalid command!");
				}
			}
		} catch (NoSuchElementException nsee) {
			System.out.println();
			System.exit(0);
		}
	}

	static class HelpCommand implements ICommand {
		public boolean matches(String input) {
			return input.toLowerCase().equals("help");
		}

		public String getUsage() {
			return "help - displays available commands";
		}

		public boolean doWork(Scanner in) {
			in.nextLine();
			// Print the usage of all commands
			for (ICommand cmd : Debugger.commands) {
				System.out.println(cmd.getUsage());
			}

			return false;
		}
	}

	static class GoCommand implements ICommand {
		public boolean matches(String input) {
			return input.toLowerCase().equals("go");
		}

		public String getUsage() {
			return "go - resume execution";	
		}

		public boolean doWork(Scanner in) {
			return true;
		}
	}

	static class ListFieldsCommand implements ICommand {
		public boolean matches(String input) {
			return input.toLowerCase().equals("fields");
		}

		public String getUsage() {
			return "fields class - list the fields of a class";
		}

		public boolean doWork(Scanner in) {
			String line = in.nextLine().trim();
			if (!ClassUtils.isValidClass(line)) {
				System.out.println("Couldn't find class: " + line);
				return false;
			}

			Class<?> c = ClassUtils.getClass(line);
			Field[] fields = c.getDeclaredFields();
			for (Field f : fields) {
				System.out.println(f.getName());
			}

			return false;
		}
	}

	static class ListMethodsCommand implements ICommand {
		public boolean matches(String input) {
			return input.toLowerCase().equals("methods");
		}

		public String getUsage() {
			return "methods class - list the methods of a class";
		}

		public boolean doWork(Scanner in) {
			String line = in.nextLine().trim();
			if (!ClassUtils.isValidClass(line)) {
				System.out.println("Couldn't find class: " + line);
				return false;
			}

			Class<?> c = ClassUtils.getClass(line);
			Method[] methods = c.getDeclaredMethods();
			for (Method m : methods) {
				System.out.println(m.toGenericString());
			}

			return false;
		}
	}

	static class QuitCommand implements ICommand {
		public boolean matches(String input) {
			return input.toLowerCase().equals("quit");
		}
		
		public String getUsage() {
			return "quit - quits the debugger (and program)";
		}

		public boolean doWork(Scanner in) {
			System.exit(0);
			return true;
		}
	}

}
