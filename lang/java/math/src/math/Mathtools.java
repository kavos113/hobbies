package math;

public class Mathtools {

    public int gcd(int a, int b) {
    	int c,d;
    	if(a < b) {
    		c = a;
    		a = b;
    		b = c;
    	}
    	d = 1;
    	while(d != 0) {
    		d = a % b;
    		a = b;
    		if(d == 0) {
    			break;
    		}
    		b = d;
    	}
    	return b;
    }

    public int lcm(int e, int f) {
    	int g = e * f;
    	int h = gcd(e,f);
    	return g/h;
    }
}
