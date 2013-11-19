package debugger;

import java.util.*;
import java.lang.reflect.*;

/**
 * The basis of the debugger. This class initializes all command objects
 * and houses the debugger prompt. This in combination with Debug.aj 
 * get the debugger going.
 */
public class Debugger {
	static final Scanner in = new Scanner(System.in);
	static final List<ICommand> commands = new LinkedList<ICommand>();
	static {
		commands.add(new HelpCommand());
		commands.add(new QuitCommand());
		commands.add(new ListFieldsCommand());
		commands.add(new ListMethodsCommand());
		commands.add(new GoCommand());

		// Force all other classes to be initialized.
		// Without this other commands won't get added 
		// to the commands list before we start
		// executing the normal program. This method
		// goes through and finds all classes in the
		// debugger package and tells java to load them.
		// This makes sure their static blocks run before
		// we do anything else.
		if (!ClassUtils.loadAllClassesInPackage("debugger")) {
			System.err.println("Failed to load debugger!");
			System.exit(1);
		}
	}

	// Interesting!
	{
		Debugger.println("CODE!");
	}

	/**
	 * Very simple command prompt. It loops through all commands in the
	 * commands ArrayList calling the matches() method until one of
	 * the commands returns a true for a match. If no commands match
	 * what was input by the user an error is reported.
	 *
	 * This method will keep looping until the doWork() method of a
	 * command returns true, meaning it has finished processing.
	 * Commands can return false instead to let another command do
	 * work once it has finished.
	 */
	public static void prompt() {
		try {
			Debugger.println("Debug prompt");
			boolean finished = false;
			while (!finished) {
				Debugger.print("> ");
				Debugger.flush();
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
					Debugger.errorln("Invalid command!");
				}
			}
		} catch (NoSuchElementException nsee) {
			Debugger.println();
			System.exit(0);
		}
	}

	//=========================================================================
	// We create our own printing functions so we can output fancy colors on
	// the terminal. This helps differentiate the debugged programs output
	// versus the debuggers output.
	//=========================================================================
	private static String NORMAL = "\u001B[0m";
	private static String BOLD_WHITE = "\u001B[37;1m";
	private static String BOLD_RED = "\u001B[31;1m";

	public static void print(Object s) {
		System.out.print(BOLD_WHITE + s.toString() + NORMAL);
	}

	public static void println(Object s) {
		print(s.toString() + "\n");
	}

	public static void println() {
		print("\n");
	}

	public static void flush() {
		System.out.flush();
	}

	public static void error(Object s) {
		System.out.print(BOLD_RED + s.toString() + NORMAL);
	}

	public static void errorln(Object s) {
		error(s.toString() + "\n");
	}

	//=========================================================================
	//=========================================================================

	/**
	 * Displays all commands and their description.
	 */
	static class HelpCommand implements ICommand {
		public boolean matches(String input) {
			return input.toLowerCase().equals("help");
		}

		public String getCommand() {
			return "help";
		}

		public String getHelp() {
			return "displays available commands";
		}

		/**
		 * Pretty prints the the commands and their descriptions.
		 *
		 * @param in The scanner for user input. Not used.
		 *
		 * @return False, to let other commands execute
		 */
		public boolean doWork(Scanner in) {
			in.nextLine();

			List<String[]> cmds = new ArrayList<String[]>(commands.size());
			int longestCommand = 0;
			for (ICommand cmd : commands) {
				String command = cmd.getCommand();
				String help = cmd.getHelp();
				cmds.add(new String[] { command, help });

				if (command.length() > longestCommand) {
					longestCommand = command.length();
				}
			}

			// Lets sort the commands alphabetically
			Collections.sort(cmds, new Comparator<String[]>() {
				public int compare(String[] s1, String[] s2) {
					return s1[0].compareTo(s2[0]);
				}
			});

			// Print the usage of all commands
			for (String[] s : cmds) {
				print(s[0]);	
				// Calculate our padding
				String padding = "";
				for (int i = 0; i < longestCommand - s[0].length(); ++i) {
					padding += " ";
				}
				print(padding);
				println(" - " + s[1]);
			}

			return false;
		}
	}

	//=========================================================================
	//=========================================================================

	/**
	 * Tells the debugger prompt to return by returning True
	 * in the doWork() function.
	 */
	static class GoCommand implements ICommand {
		public boolean matches(String input) {
			return input.toLowerCase().equals("go");
		}

		public String getHelp() {
			return "resume execution";	
		}

		public String getCommand() {
			return "go";
		}

		/**
		 * Returns true so the prompt() will return as well.
		 */
		public boolean doWork(Scanner in) {
			in.nextLine();
			return true;
		}
	}

	//=========================================================================
	//=========================================================================

	/**
	 * Lists all the fields of a class.
	 */
	static class ListFieldsCommand implements ICommand {
		public boolean matches(String input) {
			return input.toLowerCase().equals("fields");
		}

		public String getCommand() {
			return "fields class";
		}

		public String getHelp() {
			return "list the fields of a class";
		}

		/**
		 * Uses reflection to get all the fields of a class and then
		 * prints them. If the class can't be found an error message
		 * is printed instead.
		 *
		 * @param in A Scanner where the input is coming from.
		 *
		 * @return False to allow other commands to execute
		 */
		public boolean doWork(Scanner in) {
			String line = in.nextLine().trim();
			if (!ClassUtils.isValidClass(line)) {
				Debugger.errorln("Couldn't find class: " + line);
				return false;
			}

			Class<?> c = ClassUtils.getClass(line);
			Field[] fields = c.getDeclaredFields();
			for (Field f : fields) {
				Debugger.println(f.toGenericString());
			}

			return false;
		}

	}

	//=========================================================================
	//=========================================================================
	
	/**
	 * Lists all methods of a class.
	 */
	static class ListMethodsCommand implements ICommand {
		public boolean matches(String input) {
			return input.toLowerCase().equals("methods");
		}

		public String getHelp() {
			return "list the methods of a class";
		}

		public String getCommand() {
			return "methods class";
		}

		/**
		 * Uses reflection to get all the methods of a class and then
		 * prints them. If the class can't be found an error message
		 * is printed instead.
		 *
		 * @param in A Scanner where the input is coming from.
		 *
		 * @return False to allow other commands to execute
		 */
		public boolean doWork(Scanner in) {
			String line = in.nextLine().trim();
			if (!ClassUtils.isValidClass(line)) {
				Debugger.errorln("Couldn't find class: " + line);
				return false;
			}

			Class<?> c = ClassUtils.getClass(line);
			Method[] methods = c.getDeclaredMethods();
			for (Method m : methods) {
				Debugger.println(m.toGenericString());
			}

			return false;
		}
	}

	//=========================================================================
	//=========================================================================
	
	/**
	 * Quits the debugger (and program).
	 */
	static class QuitCommand implements ICommand {
		public boolean matches(String input) {
			return input.toLowerCase().equals("quit");
		}
		
		public String getHelp() {
			return "quits the debugger (and program)";
		}

		public String getCommand() {
			return "quit";
		}

		/**
		 * Calls System.exit(0)
		 *
		 * @return True, doesn't really matter
		 */
		public boolean doWork(Scanner in) {
			System.exit(0);
			return true;
		}
	}

}
