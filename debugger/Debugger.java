package debugger;

import java.util.List;
import java.util.Scanner;
import java.util.LinkedList;

public class Debugger {
	static final Scanner in = new Scanner(System.in);
	static final List<ICommand> commands = new LinkedList<ICommand>();
	static {
		commands.add(new HelpCommand());
		commands.add(new QuitCommand());
	}

	// Interesting!
	{
		System.out.println("CODE!");
	}

	public static void prompt() {
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
