package dentaku;

public class CalcEngine {
    double num1,num2;
    String ope = "eq";
    int count = 0;
    
    double result = 0;
    
    public void numInput(double num) {
    	
    	if(count == 0) {
    		num1 = num;
    		count++;
    	}else {
    		num2 = num;
    	}
    }
    
    public void opeSet(String sope) {
    	ope = sope;
    }
    
    public String keisan() {
    	if(ope.equals("tasu")) {
    		result = num1 + num2;
    		num1 = result;
    	}else if(ope.equals("hiku")) {
    		result = num1 - num2;
    		num1 = result;
    	}else if(ope.equals("kake")) {
    		result = num1 * num2;
    		num1 = result;
    	}else if(ope.equals("waru")) {
    		result = num1 / num2;
    		System.out.println(result);
    	}else {
    		result = num1;
    	}
    	
    	if(result % 1 == 0) {
    	    return String.format("%.0f", result);
    	}else {
    		return String.format("%.10f", result);
    	}
    }
    
    public void clear() {
    	num1 = 0;
    	num2 = 0;
    	result = 0;
    	ope = "eq";
    	count = 0;
    }
}
