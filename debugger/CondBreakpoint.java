package debugger;

import java.util.Scanner;
import java.util.LinkedList;
import java.lang.reflect.*;
import java.util.regex.Pattern;

public class CondBreakpoint {

	static {
		Debugger.commands.add(new BreakCondCommand());
	}

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

			parse(input);			

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
		public abstract Object evaluate();
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

		public Object evaluate() {
			return (Boolean)left.evaluate()  && (Boolean)right.evaluate();
		}
	}

	/**
	 * Or expression, true if either part is true
	 */
	static class OrExpr extends BinExpr {
		public OrExpr(Expr l, Expr r) {
			super(Token.OR, l, r);
		}

		public Object evaluate() {
			return (Boolean)left.evaluate() || (Boolean)right.evaluate();
		}
	}

	static class GtExpr extends BinExpr {
		public GtExpr(Expr l, Expr r) {
			super(Token.GT, l, r);
		}

		public Object evaluate() {
			Object lVal = left.evaluate();
			Object rVal = right.evaluate();
			return null;
		}
	}

	static class GteExpr extends BinExpr {
		public GteExpr(Expr l, Expr r) {
			super(Token.GTE, l, r);
		}

		public Object evaluate() {
			Object lVal = left.evaluate();
			Object rVal = right.evaluate();
			return null;
		}
	}

	static class LtExpr extends BinExpr {
		public LtExpr(Expr l, Expr r) {
			super(Token.LT, l, r);
		}

		public Object evaluate() {
			Object lVal = left.evaluate();
			Object rVal = right.evaluate();
			return null;
		}
	}

	static class LteExpr extends BinExpr {
		public LteExpr(Expr l, Expr r) {
			super(Token.LTE, l, r);
		}

		public Object evaluate() {
			Object lVal = left.evaluate();
			Object rVal = right.evaluate();
			return null;
		}
	}

	static class EqExpr extends BinExpr {
		public EqExpr(Expr l, Expr r) {
			super(Token.EQ, l, r);
		}

		public Object evaluate() {
			Object lVal = left.evaluate();
			Object rVal = right.evaluate();
			return null;
		}
	}

	static class ArgExpr extends Expr {
		int index;
		public ArgExpr(int index) {
			super(Token.ARG);
			this.index = index;
		}

		public Object evaluate() {
			return null;
		}
	}

	static class IntExpr extends Expr {
		int value;
		public IntExpr(int value) {
			super(Token.INT);
			this.value = value;
		}

		public Object evaluate() {
			return null;
		}
	}

	static class StrExpr extends Expr {
		String value;
		public StrExpr(String value) {
			super(Token.STR);
			this.value = value;
		}

		public Object evaluate() {
			return null;
		}
	}

	static class NullExpr extends Expr {
		public NullExpr() { 
			super(Token.NULL);
		}
		public Object evaluate() {
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

		public Object evaluate() {
			return null;
		}
	}

	//=========================================================================

	enum Token {
		AND("and", true, false, 0),
		OR("or",   true, false, 0),
		EQ("=",    true, false, 1),
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

	/**
	 * Parse a conditional expression the format is as follows:
	 *
	 * T class.name E
	 * T := f | m
	 * E := E and E | E or E | not E | V
	 * V := 
	 */
	public static void parse(String input) {
		SimpleReader in = new SimpleReader(input);			
		try {
			LinkedList<TokInfo> tokens = new LinkedList<TokInfo>();

			// TODO support fields

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

			//System.out.println("Type checking");
			// Once we've parsed it, make sure everything conforms,
			// a very simple type check
			Method m = ClassUtils.getMethod(classAndName[0], classAndName[1]);
			Class<?>[] types = m.getParameterTypes();
			if (check(types, root) != Type.BOOL) {
				throw new Exception("must provide a boolean expression!");
			}

			Debugger.println(exprToInfix(root));

		} catch (Exception e) {
			Debugger.errorln("Failed to parse conditional: " + 
				e.getMessage());
			e.printStackTrace();
			// TODO return something bad
		}
	}

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

	enum Type {
		BOOL,
		INT,
		STR,
		NULL
	};

	private static String exprToInfix(Expr node) {
		switch (node.tok) {
			case INT:  return "" + ((IntExpr)node).value;
			case STR:  return ((StrExpr)node).value;
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

	private static Type check(Class<?>[] types, Expr node) throws Exception {
		// We're using the tokens again because I'm lazy
		// also AND == BOOLEAN type for now
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

	private static Expr parseExpression(LinkedList<TokInfo> tokens) throws Exception {
		return parseExpression1(tokens, parsePrimary(tokens), 0);
	}

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
					throw new Exception("Unhandled binOp case");
			}

			if (!tokens.isEmpty()) {
				ti = tokens.get(0);
			}
		}

		return lhs;
	}
}
