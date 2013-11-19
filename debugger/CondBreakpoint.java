package debugger;

import java.util.List;
import java.util.Scanner;
import java.util.LinkedList;

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
		AND("and", true),
		OR("or", true),
		NOT("not", false),
		ARG("arg", false),
		EQ("=", true),
		LT("<", true),
		LTE("<=", true),
		GT(">", true),
		GTE(">=", true),
		LPAREN("(", false),
		RPAREN(")", false),
		NULL("null", false),
		INT("", false),
		STR("", false);

		private final String value;
		private boolean isBinOp;
		private boolean isValue;
		Token(String s, boolean binop, boolean value) {
			this.value = s;
			this.isBinOp = binop;
			this.isValue = value;
		}

		public boolean isBinOp() {
			return isBinOp;
		}

		public boolean isValue() {
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
			List<TokInfo> tokens = new LinkedList<TokInfo>();

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
				!ClassUtils.isValidMethod(classAndName[1])) {
				//(!ClassUtils.isValidMethod(classAndName[0], classAndName[1]) &&
				// !ClassUtils.isValidField(classAndName[0], classAndName[1]))) {
				throw new Exception("second token must be class.method");	
			} 

			Method m = ClassUtils.getMethod(classAndName[0], classAndName[1]);

			// Parse the rest of the tokens	
			parse(tokens, in);

			if (tokens.isEmpty()) {
				throw new Exception("Must provide an expression");
			}

			// Make sure it's surrounded by parens
			if (tokens.get(0) != Token.LPAREN) {
				tokens.insert(0, mkTok(Token.LPAREN));
				tokens.insert(tokens.size()-1, mkTok(Token.RPAREN));
			}

			// Once we've checked it, create the expression tree
			Expr root = makeTree(tokens);

			// Once we've parsed it, make sure everything conforms
			check(root);

		} catch (Exception e) {
			Debugger.errorln("Failed to parse conditional: " + 
				e.getMessage());
			// TODO return something bad
		}
	}

	private static void parse(List<TokInfo> tokens, Scanner in) throws Exception {
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

	private static void check(Expr root) {
		
	}

	private static Expr makeTree(List<TokInfo> tokens) throws Exception {
		return null;
	}

	private static Expr tokensToTree(List<TokInfo> tokens) throws Exception {
		if (tokens.isEmpty()) {
			throw new Exception("Parse error");
		}

		if (tokens.get(0).tok == Token.LPAREN || tokens.get(0).tok == Token.RPAREN) {
			throw new Exception("Internal parser error");
		}

		// If we encounter a LPAREN we have to do something special
		// Need to look ahead for binary expressions			
		if (tokens.size() > 1 && tokens.get(1).tok.isBinOp()) {
			Expr left = tokensToTree(tokens);
			TokInfo ti = tokens.remove(0);
			Expr right = tokensToTree(tokens);
			Expr e = null;
			switch (ti.tok)
			{
				case AND: return new AndExpr(left, right);
				case OR:  return new OrExpr(left, right);
				case EQ:  return new EqExpr(left, right);
				case LT:  return new LtExpr(left, right);
				case GT:  return new GtExpr(left, right);
				case GTE: return new GteExpr(left, right);
				case LTE: return new LteExpr(left, right);
			}
		}

		// We're not a binary expression
		TokInfo ti = tokens.remove(0);
		// TODO finish
		switch (ti.tok)
		{
			case NOT:  return new NotExpr(tokensToTree(tokens));
			case NULL: return new NullExpr();
			case ARG:  return new ArgExpr(((ArgInfo)ti).index);
			case INT:  return new IntExpr(((IntInfo)ti).value);
			case STR:  return new StrExpr(((StrInfo)ti).value);
			default:
				throw new Exception("Error parsing tree");
		}
	}
}
