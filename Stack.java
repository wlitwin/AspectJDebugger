// Example (script below)
//
// make stack
//> break Stack.m4
//> break Stack.m5
//> go
//> stack
//> go
//> stack
//> go
public class Stack {

	public void m1() {
		System.out.println("m1");
		m2();
	}

	public void m2() {
		System.out.println("m2");
		m3();
	}

	public void m3() {
		System.out.println("m3-1");
		m4();
		System.out.println("m3-2");
		m5();
	}

	public void m4() {
		System.out.println("m4!");	
	}

	public void m5() {
		System.out.println("m5!");
	}

	public static void main(String[] args) {
		Stack s = new Stack();
		s.m1();
	}
}
