package debugger;

import java.util.*;

public class Debugger {
	static final Scanner in = new Scanner(System.in);
	static final List<ICommand> commands = new LinkedList<ICommand>();
	static {
		commands.add(new HelpCommand());
		commands.add(new QuitCommand());
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
