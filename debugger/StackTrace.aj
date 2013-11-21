package debugger;

import java.util.*;
import org.aspectj.lang.JoinPoint;
import org.aspectj.lang.annotation.SuppressAjWarnings;

public aspect StackTrace {
	static {
		Debugger.commands.add(new StackTraceCommand());
	}

	static Stack<StackInfo> callStack = new Stack<StackInfo>();

	pointcut onCall(Object t) :
		execution(* *(..)) && target(t)
		&& !within(debugger..*);

	pointcut onGeneralCall() :
		execution(static * *(..))
		&& !within(debugger..*);

	pointcut onExecution() :
		execution(* *(..)) &&
		!within(debugger..*);

	static class StackInfo {
		String info;

		public StackInfo(JoinPoint jp) {
			this(jp, null);
		}

		private String arrayToString(Object[] o) {
			if (o.length == 0) {
				return "[]";
			}

			String info = "";
			for (int j = 0; j < o.length-1; ++j) {
				if (o[j].getClass().isArray()) {
					info += "[" + arrayToString((Object[])o[j]) + "]";
				} else {
					info += o[j].toString();	
				}

				info += ", ";
			}

			if (o[o.length-1].getClass().isArray()) {
				info += arrayToString((Object[])o[o.length-1]);
			} else {
				info += o[o.length-1].toString();
			}

			return info;
		}

		public StackInfo(JoinPoint jp, Object target) {
			String prefix = "";
			if (target != null) {
				int hashCode = System.identityHashCode(target);
				prefix = "[" + hashCode + "] ";
			}

			info = prefix + jp.getSignature().toLongString();
			Object[] args = jp.getArgs();
			if (args.length != 0) {
				info += "\n      (" + arrayToString(args) + ")";
			}
		}

		public String toString() {
			return info;
		}
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
				Debugger.println("-> The call stack is empty");
			} else {
				for (StackInfo si : callStack) {
					Debugger.println("-> " + si);
				}
			}

			return false;
		}
	}

	@SuppressAjWarnings({"adviceDidNotMatch"})
	before() : onGeneralCall() {
		callStack.add(new StackInfo((JoinPoint)thisJoinPoint));
	}

	@SuppressAjWarnings({"adviceDidNotMatch"})
	before(Object t) : onCall(t) {
		callStack.add(new StackInfo((JoinPoint)thisJoinPoint, t));
	}

	@SuppressAjWarnings({"adviceDidNotMatch"})
	after() : onExecution() {
		callStack.pop();
	}
}
