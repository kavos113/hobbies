package math;

public class Fraction {
	
	public static void main(String args[]) {
		System.out.println(keisan("1/5","1/3","tasu"));
	}
    
	public static String keisan(String str1, String str2, String ope) {
		String[] sa1,sa2;
		int[] ia1,ia2;
		int a,b,c;
		String result = null;
		Mathtools math = new Mathtools();
		
		
		sa1 = str1.split("/", 0);
		sa2 = str2.split("/", 0);
		
		ia1 = new int[2];
		ia2 = new int[2];
		
		for(int i = 0; i < 2; i++) {
			ia1[i] = Integer.parseInt(sa1[i]);
			ia2[i] = Integer.parseInt(sa2[i]);
		}
		if(ope.equals("tasu")) {
		    a = ia1[1] * ia2[1];
		    b = ia1[0] * ia2[1] + ia2[0] * ia1[1];
		    c = math.gcd(a, b);
		    a = a / c;
		    b = b / c;
		    result = b + "/" + a;
		}else if(ope.equals("hiku")) {
		    a = ia1[1] * ia2[1];
		    b = ia1[0] * ia2[1] - ia2[0] * ia1[1];
		    c = math.gcd(a, b);
		    a = a / c;
		    b = b / c;
		    result = b + "/" + a;
		}else if(ope.equals("kake")) {
			a = ia1[1] * ia2[1];
		    b = ia1[0] * ia2[0];
		    result = b + "/" + a;
		}else if(ope.equals("waru")) {
			a = ia1[1] * ia2[0];
		    b = ia1[0] * ia2[1];
		    result = b + "/" + a;
		}
		return result;
	}
}
