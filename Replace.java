/* Example (script below)
 *
 * make replace
 * > break Replace.printSomething
 * > go
 * > methods NewPrint
 * > load NewPrint.java
 * > methods NewPrint
 * > replace Replace.printSomething NewPrint.printGreen
 * > break NewPrint.printGreen
 * > breakif NewPrint.printActualGreen arg 0 = "FOOBAR" or arg 0 = "TEST"
 * > go
 * > setarg 1 CHANGED
 * > go
 * > stack
 * > setarg 1 TEST
 * > go
 * > setarg 0 FOOBAR
 * > stack
 * > go
 */
public class Replace {

	private String saved;

	public void printSomething(String word) {
		saved = word;
		System.out.println(word);
	}

	public static void main(String[] args) {
		Replace r = new Replace();	
		r.printSomething("Hello");
		r.printSomething("World");
		r.printSomething("FOOBAR");
	}
}
