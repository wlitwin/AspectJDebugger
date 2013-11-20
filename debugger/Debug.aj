package debugger;

import org.aspectj.lang.annotation.SuppressAjWarnings;

public aspect Debug {
	pointcut onMain() :	
		execution(public static void main(String[]));

	@SuppressAjWarnings({"adviceDidNotMatch"})
	before() : onMain() {
		Debugger.prompt();
	}
}
