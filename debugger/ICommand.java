package debugger;

public interface ICommand {
	/**
	 * Returns true if the command matches this command,
	 * false otherwise.
	 */
	public boolean matches(String command);

	public String getUsage();

	/*
	 */
	public boolean doWork(java.util.Scanner sc);
}
