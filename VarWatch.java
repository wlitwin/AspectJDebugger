/* Example (script below)
 *
 * make var
 * > watch VarWatch.var1
 * > watch VarWatch.var2
 * > go
 * ...
 */
public class VarWatch {
	public int var1 = 0;
	public int var2 = 0;

	public static void main(String[] args) {
		VarWatch vw = new VarWatch();
		vw.var1 = 10;
		System.out.println(vw.var2);
		vw.var2 = 20;
		System.out.println(vw.var1);
	}
}
