package debugger;

import java.util.*;
import org.aspectj.lang.annotation.SuppressAjWarnings;

public aspect StackTrace {
	static {
		Debugger.commands.add(new StackTraceCommand());
	}

	static Stack<String> callStack = new Stack<String>();

	pointcut onCall(Object t) :
		execution(* *(..)) && target(t)
		&& !within(debugger.*);

	pointcut onGeneralCall() :
		execution(static * *(..))
		&& !within(debugger.*);

	pointcut onExecution() :
		execution(* *(..)) &&
		!within(debugger.*);

	static class StackTraceCommand implements ICommand {
		public boolean matches(String input) {
			return input.toLowerCase().equals("stack");
		}

		public String getHelp() {
			return "shows a stack trace";
		}

		public String getCommand() {
			return "stack";
		}

		public boolean doWork(Scanner sc) {
			sc.nextLine(); // Consume input

			if (callStack.empty()) {
				Debugger.println("-> The call stack is empty");
			} else {
				for (String s : callStack) {
					Debugger.println("-> " + s);
				}
			}

			return false;
		}
	}

	@SuppressAjWarnings({"adviceDidNotMatch"})
	before() : onGeneralCall() {
		callStack.add(thisJoinPoint.getSignature().toLongString());
	}

	@SuppressAjWarnings({"adviceDidNotMatch"})
	before(Object t) : onCall(t) {
		int hashCode = System.identityHashCode(t);
		callStack.add("[" + hashCode + "] " + 
			thisJoinPoint.getSignature().toLongString());
	}

	@SuppressAjWarnings({"adviceDidNotMatch"})
	after() : onExecution() {
		callStack.pop();
	}
}
