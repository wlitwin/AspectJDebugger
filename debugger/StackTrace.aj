package debugger;

import java.util.*;

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

	before() : onGeneralCall() {
		callStack.add(thisJoinPoint.getSignature().toLongString());
	}

	before(Object t) : onCall(t) {
		int hashCode = System.identityHashCode(t);
		callStack.add("[" + hashCode + "] " + 
			thisJoinPoint.getSignature().toLongString());
	}

	after() : onExecution() {
		callStack.pop();
	}

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
				System.out.println("-> The call stack is empty");
			} else {
				for (String s : callStack) {
					System.out.println("-> " + s);
				}
			}

			return false;
		}
	}
}
