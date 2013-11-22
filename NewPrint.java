public class NewPrint {
	public static void printGreen(Object o, String s) {
		printActualGreen(s);
	}

	public static void printActualGreen(String s) {
		System.out.println("\u001B[32;1m" + s + "\u001B[0m");	
	}
}
