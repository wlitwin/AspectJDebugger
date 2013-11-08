package debugger;

privileged aspect VariableWatcher {
	// Need a map to see if the variable is being watched
	

	pointcut onSet(Object o, Object value):
		set(* *) && this(o) && args(value);

	// Grab the new value
	before(Object o, Object value) : onSet(o, value) {
		// Add the variable being set
		System.out.println(thisJoinPoint.getSignature());
		System.out.println(o.getClass() + " setting " + value);
	}
}
