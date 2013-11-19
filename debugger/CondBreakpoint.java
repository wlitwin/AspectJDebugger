package debugger;

import java.util.Scanner;
import java.util.LinkedList;
import java.lang.reflect.*;

public class CondBreakpoint {
	/**
	 * What all expressions must have
	 */
	static abstract class Expr {
		public abstract Object evaluate();
	}

	/**
	 * Common code for binary expressions
	 */
	static abstract class BinExpr extends Expr {
		public Expr left;
		public Expr right;
		public BinExpr(Expr left, Expr right) {
			this.left = left;
			this.right = right;
		}
	}

	/**
	 * And expression, true if both parts are true
	 */
	static class AndExpr extends BinExpr {
		public AndExpr(Expr l, Expr r) {
			super(l, r);
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
			super(l, r);
		}

		public Object evaluate() {
			return (Boolean)left.evaluate() || (Boolean)right.evaluate();
		}
	}

	static class GtExpr extends BinExpr {
		public GtExpr(Expr l, Expr r) {
			super(l, r);
		}

		public Object evaluate() {
			Object lVal = left.evaluate();
			Object rVal = right.evaluate();
			return null;
		}
	}

	static class GteExpr extends BinExpr {
		public GteExpr(Expr l, Expr r) {
			super(l, r);
		}

		public Object evaluate() {
			Object lVal = left.evaluate();
			Object rVal = right.evaluate();
			return null;
		}
	}

	static class LtExpr extends BinExpr {
		public LtExpr(Expr l, Expr r) {
			super(l, r);
		}

		public Object evaluate() {
			Object lVal = left.evaluate();
			Object rVal = right.evaluate();
			return null;
		}
	}

	static class LteExpr extends BinExpr {
		public LteExpr(Expr l, Expr r) {
			super(l, r);
		}

		public Object evaluate() {
			Object lVal = left.evaluate();
			Object rVal = right.evaluate();
			return null;
		}
	}

	static class EqExpr extends BinExpr {
		public EqExpr(Expr l, Expr r) {
			super(l, r);
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
			this.index = index;
		}

		public Object evaluate() {
			return null;
		}
	}

	static class IntExpr extends Expr {
		int value;
		public IntExpr(int value) {
			this.value = value;
		}

		public Object evaluate() {
			return null;
		}
	}

	static class StrExpr extends Expr {
		String value;
		public StrExpr(String value) {
			this.value = value;
		}

		public Object evaluate() {
			return null;
		}
	}

	static class NullExpr extends Expr {
		public NullExpr() { }
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
			this.expr = e;
		}

		public Object evaluate() {
			return null;
		}
	}

	//=========================================================================

	enum Token {
		AND("and", true, false, 0),
		OR("or", true, false, 0),
		NOT("not", false, false, 3),
		ARG("arg", false, true, 3),
		EQ("=", true, false, 2),
		LT("<", true, false, 2),
		LTE("<=", true, false, 2),
		GT(">", true, false, 2),
		GTE(">=", true, false, 2),
		LPAREN("(", false, false, 3),
		RPAREN(")", false, false, 3),
		NULL("null", false, true, 3),
		INT("", false, true, 3),
		STR("", false, true, 3);

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

	/**
	 * Read some input and make sure it's lower case
	 */
	private static String read(Scanner in) {
		return in.next().toLowerCase();
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
		Scanner in = new Scanner(input);			
		try {
			LinkedList<TokInfo> tokens = new LinkedList<TokInfo>();

			// TODO support fields

			// Read the M/F for method or field
			/*String mf = read(in);
			boolean isMethod = false;
			if (!mf.equals("f") && !mf.equals("m")) {
				throw new Exception("first token must be an m or f");
			} 

			if (mf.equals("m")) {
				isMethod = true;
			}
			*/

			// Start parsing the rest of the tokens
			String temp = read(in);	
			String[] classAndName = temp.split("\\.");
			if (classAndName.length != 2 ||
				!ClassUtils.isValidClass(classAndName[0]) ||
				!ClassUtils.isValidMethod(classAndName[0], classAndName[1])) {
				//(!ClassUtils.isValidMethod(classAndName[0], classAndName[1]) &&
				// !ClassUtils.isValidField(classAndName[0], classAndName[1]))) {
				throw new Exception("second token must be class.method");	
			} 

			if (tokens.isEmpty()) {
				throw new Exception("Must provide an expression");
			}

			// Parse the rest of the tokens	
			parse(tokens, in);

			// Make sure it's surrounded by parens
			if (tokens.get(0).tok != Token.LPAREN) {
				tokens.addFirst(mkTok(Token.LPAREN));
				tokens.addLast(mkTok(Token.RPAREN));
			}

			// Check for balanced parentheses
			if (!parensMatched(tokens)) {
				throw new Exception("Unbalanced parentheses");
			}

			// Once we've checked it, create the expression tree
			Expr root = parsePrimary(tokens);

			// Once we've parsed it, make sure everything conforms,
			// a very simple type check
			Method m = ClassUtils.getMethod(classAndName[0], classAndName[1]);
			Type[] types = m.getGenericParameterTypes();
			check(types, root);

		} catch (Exception e) {
			Debugger.errorln("Failed to parse conditional: " + 
				e.getMessage());
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

	private static Token check(Type[] types, Expr node) throws Exception {
		// We're using the tokens again because I'm lazy
		// also AND == BOOLEAN type for now

		if (node instanceof IntExpr) {
			return Token.INT; 
		} else if (node instanceof StrExpr) {
			return Token.STR;
		} else if (node instanceof NullExpr) {
			return Token.NULL;
		} else if (node instanceof NotExpr) {
			Token exprType = check(types, ((NotExpr)node).expr);
			if (exprType != Token.AND) {
				throw new Exception("Not must be applied to a boolean expression!");
			}
			return Token.AND;
		} else if (node instanceof ArgExpr) {
			ArgExpr ae = (ArgExpr) node;
			if (ae.index < 0 || ae.index >= types.length) {
				throw new Exception("Invalid argument index: " + ae.index);
			}
			Type t = types[ae.index];
			if (t == String.class) {
				return Token.STR;
			} else if (t == Integer.class) {
				return Token.INT;
			} else {
				throw new Exception("Can't use arg for this parameter index: " + ae.index + " it has type: " + t);
			}
		} else if (node instanceof BinExpr) {
			Token left = check(types, ((BinExpr)node).left);
			Token right = check(types, ((BinExpr)node).right);

			if (node instanceof AndExpr) {
				if (left != Token.AND || right != Token.AND) {
					throw new Exception("And must be applied to two boolean expressions!");
				}
				return Token.AND;
			} else if (node instanceof OrExpr) {
				if (left != Token.AND || right != Token.AND) {
					throw new Exception("Or must be applied to two boolean expressions!");
				}
			} else if (node instanceof EqExpr) {
				if (left == right || 
					(left == Token.STR && right == Token.NULL) ||
					(left == Token.NULL && right == Token.STR)) {
					return Token.AND;
				} else {
					throw new Exception("= must be applied to two expressions of the same type!");
				}
			} else if (node instanceof LteExpr) {
				if (left != right) {
					throw new Exception("<= must be applied to two expressions of the same type!");
				}
				return Token.AND;
			} else if (node instanceof GteExpr) {
				if (left != right) {
					throw new Exception(">= must be applied to two expressions of the same type!");
				}
				return Token.AND;
			} else if (node instanceof LtExpr) {
				if (left != right) {
					throw new Exception("< must be applied to two expressions of the same type!");
				}
				return Token.AND;
			} else if (node instanceof GtExpr) {
				if (left != right) {
					throw new Exception("> must be applied to two expressions of the same type!");
				}
				return Token.AND;
			} else {
				throw new Exception("Type check missing BinOp case");
			}
		}

		throw new Exception("Type check missing case");
	}

	private static void parse(LinkedList<TokInfo> tokens, Scanner in) throws Exception {
		while (in.hasNext()) {
			String val = read(in);

			while (val.startsWith(Token.LPAREN.value())) {
				tokens.add(mkTok(Token.LPAREN));
				val = val.substring(1);
			}

			while (val.endsWith(Token.RPAREN.value())) {
				tokens.add(mkTok(Token.RPAREN));
				val = val.substring(0, val.length() - 1);
			}

			if (val.equals(Token.AND.value())) { tokens.add(mkTok(Token.AND)); }
			else if (val.equals(Token.OR.value()))  { tokens.add(mkTok(Token.OR));  }
			else if (val.equals(Token.EQ.value()))  { tokens.add(mkTok(Token.EQ));  }
			else if (val.equals(Token.NOT.value())) { tokens.add(mkTok(Token.NOT)); }
			else if (val.equals(Token.GT.value()))  { tokens.add(mkTok(Token.GT));  }
			else if (val.equals(Token.GTE.value())) { tokens.add(mkTok(Token.GTE)); }
			else if (val.equals(Token.LT.value()))  { tokens.add(mkTok(Token.LT));  }
			else if (val.equals(Token.LTE.value())) { tokens.add(mkTok(Token.LTE)); }
			else if (val.equals(Token.ARG.value())) {
			  	int index = Integer.parseInt(read(in));
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
		while (ti.tok.isBinOp() && ti.tok.precedence() >= min_precedence) {
			TokInfo op = tokens.remove(0);	
			Expr rhs = parsePrimary(tokens);
			ti = tokens.get(0);
			while (ti.tok.isBinOp() && ti.tok.precedence() > op.tok.precedence()) {
				TokInfo lookAhead = tokens.get(0);
				rhs = parseExpression1(tokens, rhs, lookAhead.tok.precedence());
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
		}

		return lhs;
	}
}
