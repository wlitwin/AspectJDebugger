package debugger;

public aspect Debug {
	pointcut onMain() :	
		execution(public static void main(String[]));

	before() : onMain() {
		System.out.println("Before main!");
		Debugger.prompt();
	}
}
