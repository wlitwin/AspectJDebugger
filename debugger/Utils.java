package debugger;

/**
 * Contains some helper functions to be used by the entire
 * debugger package.
 */
public class Utils {
	/**
	 * Determine if a string can be converted to a number
	 *
	 * @param str The string to check
	 *
	 * @return True if str can successfully be converted
	 */
	public static boolean isInteger(String str) {
		try {
			Integer.parseInt(str);
			return true;
		} catch (Exception e) {
			return false;
		}
	}
}
