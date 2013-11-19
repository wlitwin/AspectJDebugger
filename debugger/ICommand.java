package debugger;

/**
 * Specifies the interface for all debugger commands.
 */
public interface ICommand {
	/**
	 * Returns true if the command matches this command,
	 * false otherwise.
	 *
	 * @param command The command to check against
	 *
	 * @return See description
	 */
	public boolean matches(String command);

	/**
	 * Returns a simple usage string like:
	 *
	 *    break class.method
	 *
	 * @return A usage string
	 */
	public String getCommand();

	/**
	 * Returns a short string detailing the functionality of the
	 * command
	 *
	 * @return See description
	 */
	public String getHelp();

	/** This is called if matches() returned true. The
	 * scanner will be positions just after the string
	 * that matches checked. So it can be used for getting
	 * any additional arguments the user entered.
	 *
	 * All commands should at least call sc.nextLine()
	 * to clear the current line, even if they do not
	 * use that input. This makes the debugger well
	 * behaved for other commands.
	 *
	 * @param sc A scanner to retrieve additional information
	 *
	 * @return True if the debugger prompt should return and
	 *         allow the program to run. Most commands will
	 *         return false to allow other commands to run.
	 */
	public boolean doWork(java.util.Scanner sc);
}
