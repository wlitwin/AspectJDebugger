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
			System.out.println("Debug prompt");
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

		public String getCommand() {
			return "help";
		}

		public String getHelp() {
			return "displays available commands";
		}

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
				System.out.print(s[0]);	
				// Calculate our padding
				String padding = "";
				for (int i = 0; i < longestCommand - s[0].length(); ++i) {
					padding += " ";
				}
				System.out.print(padding);
				System.out.println(" - " + s[1]);
			}

			return false;
		}
	}

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

		public boolean doWork(Scanner in) {
			return true;
		}
	}

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

		public String getHelp() {
			return "list the methods of a class";
		}

		public String getCommand() {
			return "methods class";
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
		
		public String getHelp() {
			return "quits the debugger (and program)";
		}

		public String getCommand() {
			return "quit";
		}

		public boolean doWork(Scanner in) {
			System.exit(0);
			return true;
		}
	}

}
