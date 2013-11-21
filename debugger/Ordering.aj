package debugger;

aspect Ordering {
	declare precedence: Debug, Replacer, Breakpoint, StackTrace, VariableWatcher, Instances;
}
