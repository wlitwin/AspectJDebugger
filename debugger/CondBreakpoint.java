package debugger;

import java.util.*;
import java.lang.reflect.*;

/**
 * Handles conditional expressions. It also provides the implementation
 * for two debugger commands: BreakCondCommand and BreakCondRemoveCommand
 */
public class CondBreakpoint {
	private Expr root;
	private String combinedName;

	/**
	 * Stores a parsed conditional expression and the class and method name.
	 *
	 * @param combinedName The class and method name in the format: class.name
	 *
	 * @param root A parsed (and valid) conditional expression
	 */
	public CondBreakpoint(String combinedName, Expr root) {
		this.root = root;
		this.combinedName = combinedName;
	}

	/**
	 * Evaluate the expression with the given arguments. Call this to determine
	 * if a breakpoint should happen.
	 *
	 * @param args The parameters to the method
	 *
	 * @return True if the expression evaluated to true, false otherwise
	 */
	public boolean evaluate(Object[] args) {
		return (Boolean)root.evaluate(args);
	}

	//=========================================================================
	//=========================================================================

	/**
	 * Implementation for the command to create a new conditional breakpoint.
	 */
	static class BreakCondCommand implements ICommand {
		public boolean matches(String input) {
			return input.toLowerCase().equals("breakif");	
		}

		public String getHelp() {
			return "conditional breakpoint";
		}

		public String getCommand() {
			return "breakif class.method expression";
		}

		public boolean doWork(Scanner in) {
			String input = in.nextLine();

			CondBreakpoint cb = parse(input);
			if (cb != null) {
				Map<String, CondBreakpoint> bpMap = Breakpoint.condBreakpoints;
				if (bpMap.containsKey(cb.combinedName)) {
					Debugger.println("Updating breakpoint");
				} else {
					Debugger.println("Adding breakpoint");
				}

				bpMap.put(cb.combinedName, cb);
			}

			return false;
		}
	}

	//=========================================================================
	//=========================================================================

	/**
	 * Implementation of the command to remove an existing breakpoint.
	 */
	static class BreakCondRemoveCommand implements ICommand {
		public boolean matches(String input) {
			return input.toLowerCase().equals("breakifremove");
		}

		public String getHelp() {
			return "removes a conditional breakpoint";
		}

		public String getCommand() {
			return "breakifremove class.method";
		}

		public boolean doWork(Scanner in) {
			String line = in.nextLine();
			String[] can = line.split("\\.");
			if (can.length != 2) {
				Debugger.errorln("Invalid usage: " + getCommand());
				return false;
			}

			if (!ClassUtils.isValidClass(can[0])) {
				Debugger.errorln("No such class: " + can[0]);
				return false;
			}

			if (!ClassUtils.isValidMethod(can[0], can[1])) {
				Debugger.errorln("Class: " + can[0] + " has no such method: " + can[1]);
				return false;
			}

			Map<String, CondBreakpoint> bpMap = Breakpoint.condBreakpoints;
			String combined = can[0] + "." + can[1];
			if (!bpMap.containsKey(combined)) {
				Debugger.errorln("No conditional breakpoint for: " + combined);
				return false;
			}

			bpMap.remove(combined);
			Debugger.println("Removed breakpoint: " + combined);

			return false;
		}
	}

	//=========================================================================
	// Below this is the code for parsing the conditional expressions
	//========================================================================

	/**
	 * What all expressions must have
	 */
	static abstract class Expr {
		public Token tok;
		public Expr(Token tok) {
			this.tok = tok;
		}
		public abstract Object evaluate(Object[] args);
	}

	/**
	 * Common code for binary expressions
	 */
	static abstract class BinExpr extends Expr {
		public Expr left;
		public Expr right;
		public BinExpr(Token tok, Expr left, Expr right) {
			super(tok);
			this.left = left;
			this.right = right;
		}
	}

	/**
	 * And expression, true if both parts are true
	 */
	static class AndExpr extends BinExpr {
		public AndExpr(Expr l, Expr r) {
			super(Token.AND, l, r);
		}

		public Object evaluate(Object[] args) {
			return (Boolean)left.evaluate(args)  && (Boolean)right.evaluate(args);
		}
	}

	/**
	 * Or expression, true if either part is true
	 */
	static class OrExpr extends BinExpr {
		public OrExpr(Expr l, Expr r) {
			super(Token.OR, l, r);
		}

		public Object evaluate(Object[] args) {
			return (Boolean)left.evaluate(args) || (Boolean)right.evaluate(args);
		}
	}

	/**
	 * Greater than expression, true if compareTo returns 1
	 */
	static class GtExpr extends BinExpr {
		public GtExpr(Expr l, Expr r) {
			super(Token.GT, l, r);
		}

		public Object evaluate(Object[] args) {
			Comparable lVal = (Comparable)left.evaluate(args);
			Comparable rVal = (Comparable)right.evaluate(args);

			return lVal.compareTo(rVal) > 0;
		}
	}

	/**
	 * Greater than or equal to expression, true if compareTo
	 * returns 0 or 1
	 */
	static class GteExpr extends BinExpr {
		public GteExpr(Expr l, Expr r) {
			super(Token.GTE, l, r);
		}

		public Object evaluate(Object[] args) {
			Comparable lVal = (Comparable)left.evaluate(args);
			Comparable rVal = (Comparable)right.evaluate(args);

			return lVal.compareTo(rVal) >= 0;
		}
	}

	/**
	 * Less than expression, return true if compreTo returns -1
	 */
	static class LtExpr extends BinExpr {
		public LtExpr(Expr l, Expr r) {
			super(Token.LT, l, r);
		}

		public Object evaluate(Object[] args) {
			Comparable lVal = (Comparable)left.evaluate(args);
			Comparable rVal = (Comparable)right.evaluate(args);

			return lVal.compareTo(rVal) < 0;
		}
	}

	/**
	 * Less than or equal to expression, returns true if compareTo
	 * returns -1 or 0
	 */
	static class LteExpr extends BinExpr {
		public LteExpr(Expr l, Expr r) {
			super(Token.LTE, l, r);
		}

		public Object evaluate(Object[] args) {
			Comparable lVal = (Comparable)left.evaluate(args);
			Comparable rVal = (Comparable)right.evaluate(args);

			return lVal.compareTo(rVal) <= 0;
		}
	}

	/**
	 * Equal expression, returns true if compareTo returns 0
	 */
	static class EqExpr extends BinExpr {
		public EqExpr(Expr l, Expr r) {
			super(Token.EQ, l, r);
		}

		public Object evaluate(Object[] args) {
			Comparable lVal = (Comparable)left.evaluate(args);
			Comparable rVal = (Comparable)right.evaluate(args);

			return lVal.compareTo(rVal) == 0;
		}
	}

	/**
	 * Argument expression, returns an object from the args array
	 * using the saved index
	 */
	static class ArgExpr extends Expr {
		int index;
		public ArgExpr(int index) {
			super(Token.ARG);
			this.index = index;
		}

		public Object evaluate(Object[] args) {
			return args[index];
		}
	}

	/**
	 * Integer value, holds an integer constant
	 */
	static class IntExpr extends Expr {
		int value;
		public IntExpr(int value) {
			super(Token.INT);
			this.value = value;
		}

		public Object evaluate(Object[] args) {
			return value;
		}
	}

	/**
	 * String value, holds a string constant
	 */
	static class StrExpr extends Expr {
		String value;
		public StrExpr(String value) {
			super(Token.STR);
			this.value = value;
		}

		public Object evaluate(Object[] args) {
			return value;
		}
	}

	/**
	 * Null expression, represents null
	 */
	static class NullExpr extends Expr {
		public NullExpr() { 
			super(Token.NULL);
		}

		public Object evaluate(Object[] args) {
			return null;
		}
	}

	/**
	 * Not expression, inverts the underlying expression
	 */
	static class NotExpr extends Expr {
		Expr expr;
		public NotExpr(Expr e) {
			super(Token.NOT);
			this.expr = e;
		}

		public Object evaluate(Object[] args) {
			return !((Boolean)expr.evaluate(args));
		}
	}

	//=========================================================================
	//=========================================================================

	/**
	 * These are the tokens that make up the mini conditional language
	 */
	enum Token {
		AND("and", true, false, 0), // Lowest precedence
		OR("or",   true, false, 0),
		EQ("=",    true, false, 1), // Highest precedence
		LT("<",    true, false, 1),
		LTE("<=",  true, false, 1),
		GT(">",    true, false, 1),
		GTE(">=",  true, false, 1),
		// Primaries don't have a precedence
		NOT("not", false, false, -1),
		ARG("arg", false, true, -1),
		LPAREN("(", false, false, -1),
		RPAREN(")", false, false, -1),
		NULL("null", false, true, -1),
		INT("", false, true, -1),
		STR("", false, true, -1);

		private final String value;
		private boolean isBinOp;
		private boolean isValue;
		private int precedence;

		Token(String s, boolean binop, boolean value, int precedence) {
			this.value = s;
			this.isBinOp = binop;
			this.isValue = value;
			this.precedence = precedence;
		}

		public boolean isBinOp() {
			return isBinOp;
		}

		public boolean isValue() {
			return isValue;
		}

		public int precedence() {
			return precedence;
		}

		public String value() {
			return value;
		}
	}

	//=========================================================================
	// These types mirror the Token enum and store some additional information
	// needed by the parser at later stages
	//=========================================================================

	static class TokInfo {
		public Token tok;	
		public TokInfo(Token tok) {
			this.tok = tok;
		}
	}

	static class ArgInfo extends TokInfo {
		public int index;
		public ArgInfo(int index) {
			super(Token.ARG);
			this.index = index;
		}
	}

	static class IntInfo extends TokInfo {
		public int value;
		public IntInfo(int value) {
			super(Token.INT);
			this.value = value;
		}
	}

	static class StrInfo extends TokInfo {
		public String value;	
		public StrInfo(String value) {
			super(Token.STR);
			this.value = value;
		}
	}

	private static TokInfo mkTok(Token t) {
		return new TokInfo(t);
	}

	//=========================================================================
	// Simple scanner to help read the expression string
	//=========================================================================

	static class SimpleReader {
		private String input;
		private int index = 0;
		public SimpleReader(String input) {
			this.input = input;
		}

		public boolean hasNext() {
			return index != input.length();
		}

		private boolean isDelimiter(char c) {
			return c == '(' || c == ')';
		}

		public String next() {
			char c = input.charAt(index);
			while (Character.isWhitespace(c)) {
				++index;
				if (!hasNext()) {
					throw new RuntimeException("Reached end of input too soon");
				}
				c = input.charAt(index);
			}

			if (isDelimiter(c)) {
				++index;
				return "" + c;
			}

			String next = "";
			while (!Character.isWhitespace(c) && !isDelimiter(c) && hasNext()) {
				next += c;
				++index;
				if (!hasNext()) break;
				c = input.charAt(index);
			}

			return next;
		}
	}

	//=========================================================================
	// Parsing
	//=========================================================================
	
	/**
	 * Parse a conditional expression the grammar is as follows:
	 *
	 * E := G and G | G or G 
	 * G := H > H | H < H | H >= H | H <= H
	 * H := not E | ( E ) | V
	 * V := null | integer | string
	 */
	public static CondBreakpoint parse(String input) {
		SimpleReader in = new SimpleReader(input);			
		try {
			LinkedList<TokInfo> tokens = new LinkedList<TokInfo>();

			// Start parsing the rest of the tokens
			String temp = in.next();
			String[] classAndName = temp.split("\\.");
			if (classAndName.length != 2) {
				throw new Exception("Must specify a class.method");
			}

			if (!ClassUtils.isValidClass(classAndName[0])) {
				throw new Exception("No such class: " + classAndName[0]);
			}

			if (!ClassUtils.isValidMethod(classAndName[0], classAndName[1])) {
				throw new Exception("No such method: " + classAndName[1] 
					+ " for class: " + classAndName[0]);	
			} 

			// Parse the rest of the tokens	
			parse(tokens, in);

			if (tokens.isEmpty()) {
				throw new Exception("Must provide an expression");
			}

			//System.out.println("Parsed tokens: " + tokens.size());

			// Check for balanced parentheses
			if (!parensMatched(tokens)) {
				throw new Exception("Unbalanced parentheses");
			}

			/*for (TokInfo ti : tokens) {
				System.out.print(ti.tok.name() + " ");	
			}
			System.out.println();
			*/

			// Once we've checked it, create the expression tree
			Expr root = parseExpression(tokens);

			// Once we've parsed it, make sure everything conforms,
			// a very simple type check
			Method m = ClassUtils.getMethod(classAndName[0], classAndName[1]);
			Class<?>[] types = m.getParameterTypes();
			if (check(types, root) != Type.BOOL) {
				throw new Exception("Must provide a boolean expression!");
			}

			Debugger.println(exprToInfix(root));

			return new CondBreakpoint(classAndName[0] + "." + classAndName[1], root);
		} catch (Exception e) {
			Debugger.errorln("Failed to parse conditional: " + e.getMessage());
		}

		return null;
	}

	/**
	 * Does most of the leg work of parsing the string and creating a list of tokens.
	 *
	 * @param tokens A list to add tokens to
	 *
	 * @param in A scanner that knows the proper delimiters
	 *
	 * @throws Exception If an invalid token is found
	 */
	private static void parse(LinkedList<TokInfo> tokens, SimpleReader in) throws Exception {
		while (in.hasNext()) {
			String val = in.next();
			//System.out.println("Got token: " + val);

			if (val.equals(Token.LPAREN.value()))   { tokens.add(mkTok(Token.LPAREN)); }
			else if (val.equals(Token.RPAREN.value())) { tokens.add(mkTok(Token.RPAREN)); }
			else if (val.equals(Token.AND.value()))  { tokens.add(mkTok(Token.AND));  }
			else if (val.equals(Token.OR.value()))   { tokens.add(mkTok(Token.OR));   }
			else if (val.equals(Token.EQ.value()))   { tokens.add(mkTok(Token.EQ));   }
			else if (val.equals(Token.NOT.value()))  { tokens.add(mkTok(Token.NOT));  }
			else if (val.equals(Token.GT.value()))   { tokens.add(mkTok(Token.GT));   }
			else if (val.equals(Token.GTE.value()))  { tokens.add(mkTok(Token.GTE));  }
			else if (val.equals(Token.LT.value()))   { tokens.add(mkTok(Token.LT));   }
			else if (val.equals(Token.LTE.value()))  { tokens.add(mkTok(Token.LTE));  }
			else if (val.equals(Token.NULL.value())) { tokens.add(mkTok(Token.NULL)); }
			else if (val.equals(Token.ARG.value())) {
			  	int index = Integer.parseInt(in.next());
			  	tokens.add(new ArgInfo(index));
			} else if (Utils.isInteger(val)) {
				int value = Integer.parseInt(val);
				tokens.add(new IntInfo(value));
			} else if (val.startsWith("\"") && val.endsWith("\"")) {
				tokens.add(new StrInfo(val.substring(1, val.length()-1)));
			} else {
				throw new Exception("Invalid token: " + val);
			}
		}
	}

	//=========================================================================
	// AST creation
	//=========================================================================

	/**
	 * This is the method that should be called with a freshly tokenized input.
	 *
	 * Note: This is destructive. The tokens list will be empty if successfully
	 *       converted to an AST.
	 *
	 * @param tokens The starting list of tokens to turn into an AST
	 *
	 * @throws Exception If it could not correctly create the AST
	 *
	 * @return The root node of the AST
	 */
	private static Expr parseExpression(LinkedList<TokInfo> tokens) throws Exception {
		return parseExpression1(tokens, parsePrimary(tokens), 0);
	}

	/**
	 * Read a primary value from the tokens list and turn it into an AST node. This
	 * method also handles parentheses.
	 *
	 * Note: This is destructive. The tokens list will be modified.
	 *
	 * @param tokens The remaining list of tokens
	 *
	 * @throws Exception If a non-primary token is found
	 */
	private static Expr parsePrimary(LinkedList<TokInfo> tokens) throws Exception {
		if (tokens.isEmpty()) {
			throw new Exception("Parse error");
		}

		TokInfo ti = tokens.remove(0);
		if (ti.tok == Token.LPAREN) {
			Expr e = parseExpression(tokens);
			ti = tokens.remove(0);
			if (ti.tok != Token.RPAREN) {
				throw new Exception("Bad parens");
			}
			return e;
		} else if (ti.tok == Token.INT) {
			return new IntExpr(((IntInfo)ti).value);
		} else if (ti.tok == Token.ARG) {
			return new ArgExpr(((ArgInfo)ti).index);
		} else if (ti.tok == Token.STR) {
			return new StrExpr(((StrInfo)ti).value);
		} else if (ti.tok == Token.NULL) {
			return new NullExpr();
		} else if (ti.tok == Token.NOT) {
			return new NotExpr(parseExpression(tokens));
		}

		throw new Exception("Parse Primary - Parse Error");
	}

	/**
	 * Helper function for correctly parsing (most) precedence based LR(1) grammars. Pseudo code from
	 * http://en.wikipedia.org/wiki/Operator-precedence_parser
	 *
	 * Note: This list removes elements from the tokens list
	 *
	 * @param tokens The remaining list of tokens
	 *
	 * @param lhs The left hand side expression
	 *
	 * @param min_precedence The current minimum precedence, used to determine when to reduce or shift
	 *
	 * @throws Exception Some of the methods this calls throw exceptions
	 *
	 * @return A correctly formed expression tree
	 */
	private static Expr parseExpression1(LinkedList<TokInfo> tokens, Expr lhs, int min_precedence) throws Exception {
		TokInfo ti = tokens.get(0);
		while (!tokens.isEmpty() && ti.tok.isBinOp() && ti.tok.precedence() >= min_precedence) {
			TokInfo op = tokens.remove(0);	
			Expr rhs = parsePrimary(tokens);
			if (!tokens.isEmpty()) {
				ti = tokens.get(0);
				while (ti.tok.isBinOp() && ti.tok.precedence() > op.tok.precedence()) {
					TokInfo lookAhead = tokens.get(0);
					rhs = parseExpression1(tokens, rhs, lookAhead.tok.precedence());

					if (tokens.isEmpty()) break;
					ti = tokens.get(0);
				}
			}

			switch (op.tok) {
				case AND: lhs = new AndExpr(lhs, rhs); break;
				case OR:  lhs = new OrExpr(lhs, rhs);  break;
				case EQ:  lhs = new EqExpr(lhs, rhs);  break;
				case LT:  lhs = new LtExpr(lhs, rhs);  break;
				case GT:  lhs = new GtExpr(lhs, rhs);  break;
				case LTE: lhs = new LteExpr(lhs, rhs); break;
				case GTE: lhs = new GteExpr(lhs, rhs); break;
				default:
					throw new RuntimeException("Unhandled binOp case");
			}

			if (!tokens.isEmpty()) {
				ti = tokens.get(0);
			}
		}

		return lhs;
	}

	//=========================================================================
	// Helper methods
	//=========================================================================

	/**
	 * Verify that the parentheses are balanced in the expression.
	 *
	 * @param tokens A list of tokens
	 *
	 * @return True if the parentheses are balanced, false otherwise
	 */
	private static boolean parensMatched(LinkedList<TokInfo> tokens) {
		int lparens = 0;	
		for (TokInfo ti : tokens) {
			if (ti.tok == Token.LPAREN) {
				++lparens;
			} else if (ti.tok == Token.RPAREN) {
				--lparens;
				if (lparens < 0) {
					return false;
				}
			}
		}

		return lparens == 0;
	}

	/**
	 * Convert an expression tree to an infix string representation. Includes all parentheses
	 * to make the order of operations clear.
	 *
	 * @param node The root of the expression tree
	 *
	 * @return An infix string of the expression tree
	 */
	private static String exprToInfix(Expr node) {
		switch (node.tok) {
			case INT:  return "" + ((IntExpr)node).value;
			case STR:  return "\"" + ((StrExpr)node).value + "\"";
			case NULL: return "null";
			case ARG:  return "arg[" + ((ArgExpr)node).index + "]";
			case NOT:  return "not (" + exprToInfix(((NotExpr)node).expr) + ")";
			case AND:  return "(" + exprToInfix(((BinExpr)node).left) + " && " + exprToInfix(((BinExpr)node).right) + ")";
			case OR:   return "(" + exprToInfix(((BinExpr)node).left) + " || " + exprToInfix(((BinExpr)node).right) + ")";
			case LT:   return "(" + exprToInfix(((BinExpr)node).left) + " < "  + exprToInfix(((BinExpr)node).right) + ")";
			case LTE:  return "(" + exprToInfix(((BinExpr)node).left) + " <= " + exprToInfix(((BinExpr)node).right) + ")";
			case GT:   return "(" + exprToInfix(((BinExpr)node).left) + " > "  + exprToInfix(((BinExpr)node).right) + ")";
			case GTE:  return "(" + exprToInfix(((BinExpr)node).left) + " >= " + exprToInfix(((BinExpr)node).right) + ")";
			case EQ:   return "(" + exprToInfix(((BinExpr)node).left) + " = "  + exprToInfix(((BinExpr)node).right) + ")";
			default:
				throw new RuntimeException("Unhandled infix case");
		}
	}

	//=========================================================================
	// Type checking
	//=========================================================================

	/**
	 * The types of the expressions. Used for typechecking
	 */
	enum Type {
		BOOL,
		INT,
		STR,
		NULL
	};

	/**
	 * Verifies that the tree is correctly formed with respected to types and
	 * ensures that argument indices are not out of bounds.
	 *
	 * @param types The types of each of the method's arguments
	 *
	 * @param node The root node of the expression tree
	 *
	 * @return The final type that the expression evaluates to
	 */
	private static Type check(Class<?>[] types, Expr node) throws Exception {
		switch (node.tok) {
			case INT: return Type.INT;
			case STR: return Type.STR;
			case NULL: return Type.NULL;
			case NOT:
			{
				if (check(types, ((NotExpr)node).expr) != Type.BOOL) {
					throw new Exception("Not must be applied to a boolean expression!");
				}
				return Type.BOOL;
			}
			case ARG:
			{
				ArgExpr ae = (ArgExpr) node;
				if (ae.index < 0 || ae.index >= types.length) {
					throw new Exception("Invalid argument index: " + ae.index);
				}
				Class<?> t = types[ae.index];
				if (String.class.isAssignableFrom(t)) {
					return Type.STR;
				} else if (Integer.class.isAssignableFrom(t) || 
						int.class.isAssignableFrom(t)) {
					return Type.INT;
				} else {
					throw new Exception("Can't use arg for this parameter [index: " + ae.index + "] it has type " + t);
				}
			}
			case AND:
			case OR:
			{
				String name = "Or";
				if (node.tok == Token.AND) { name = "And"; }
				Type left = check(types, ((BinExpr)node).left);
				Type right = check(types, ((BinExpr)node).right);
				if (left != Type.BOOL || right != Type.BOOL) {
					throw new Exception(name + " must be applied to two boolean expressions!");
				}

				return Type.BOOL;
			}
			case LT:
			case GT:
			case LTE:
			case GTE:
			{
				String name = "<";
				if (node.tok == Token.GT) { name = ">"; }
				else if (node.tok == Token.LTE) { name = "<="; }
				else if (node.tok == Token.GTE) { name = ">="; }

				Type left = check(types, ((BinExpr)node).left);
				Type right = check(types, ((BinExpr)node).right);

				if (left != right) {
					throw new Exception(name + " must be applied to two expressions of the same type!");
				}

				return Type.BOOL;
			}
			case EQ:
			{
				Type left = check(types, ((BinExpr)node).left);
				Type right = check(types, ((BinExpr)node).right);

				if (left == right || 
					(left == Type.STR && right == Type.NULL) ||
					(left == Type.NULL && right == Type.STR)) {
					return Type.BOOL;
				} else {
					throw new Exception("= can only be used on two expressions of the same type!");
				}
			}
			default:
				throw new Exception("Unhandled token type!");
		}
	}
}
