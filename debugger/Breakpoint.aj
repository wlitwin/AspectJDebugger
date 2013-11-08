/* Implements the breakpoint feature for the debugger
 */
package debugger;

import java.util.Map;
import java.util.HashMap;
import org.aspectj.lang.ProceedingJoinPoint;
import org.aspectj.runtime.internal.AroundClosure;
//import org.aspectj.runtime.reflect.JoinPointImpl;
import java.lang.reflect.*;
privileged aspect Breakpoint {
	Map<String, Boolean> breakpoints = new HashMap<String, Boolean>();	

	// Unfortunately this seems to be the only way to do it
	pointcut methodCall(): 
		execution(* **())
		&& !within(debugger.*);
	pointcut methodCall1(Object o):
		execution(* **(..)) && args(o)
		&& !within(debugger.*);
	pointcut methodCall2(Object o1, Object o2):
		execution(* **(..)) && args(o1, o2)
		&& !within(debugger.*);
	pointcut methodCall3(Object o1, Object o2, Object o3):
		execution(* **(..)) && args(o1, o2, o3)
		&& !within(debugger.*);
	pointcut methodCall4(Object o1, Object o2, Object o3, Object o4):
		execution(* **(..)) && args(o1, o2, o3, o4)
		&& !within(debugger.*);
	pointcut methodCall5(Object o1, Object o2, Object o3, Object o4, Object o5):
		execution(* **(..)) && args(o1, o2, o3, o4, o5)
		&& !within(debugger.*);
	
	Object[] breakPoint(Object[] args) {
		return args;
	}

	Object around() : methodCall() {
		System.out.println("0 " + thisJoinPointStaticPart.getSignature().toLongString());
		breakPoint(new Object[] { });
		return proceed();	
	}

	Object around(Object o) : methodCall1(o) {
		System.out.println("1 " + thisJoinPointStaticPart.getSignature().toLongString());
		Object[] new_args = breakPoint(new Object[] { o });
		return proceed(new_args[0]);
	}

	Object around(Object o1, Object o2) : methodCall2(o1, o2) {
		System.out.println("2 " + thisJoinPointStaticPart.getSignature().toLongString());
		Object[] new_args = breakPoint(new Object[] { o1, o2 });
		return proceed(new_args[0], new_args[1]);
	}

	Object around(Object o1, Object o2, Object o3) : methodCall3(o1, o2, o3) {
		System.out.println("3 " + thisJoinPointStaticPart.getSignature().toLongString());
		Object[] new_args = breakPoint(new Object[] { o1, o2, o3 });
		return proceed(new_args[0], new_args[1], new_args[2]);
	}

	Object around(Object o1, Object o2, Object o3, Object o4) : methodCall4(o1, o2, o3, o4) {
		System.out.println("4 " + thisJoinPointStaticPart.getSignature().toLongString());
		Object[] new_args = breakPoint(new Object[] { o1, o2, o3, o4 });
		return proceed(new_args[0], new_args[1], new_args[2], new_args[3]);
	}

	Object around(Object o1, Object o2, Object o3, Object o4, Object o5) : methodCall5(o1, o2, o3, o4, o5) {
		System.out.println("5 " + thisJoinPointStaticPart.getSignature().toLongString());
		Object[] new_args = breakPoint(new Object[] { o1, o2, o3, o4, o5 });
		return proceed(new_args[0], new_args[1], new_args[2], new_args[3], new_args[4]);
	}

}
