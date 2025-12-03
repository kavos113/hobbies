public class Vehicle {
    private int speed;
    private int angle;
    private final String ownerName;
    private final int ID;

    static int nextID = 0;

    public Vehicle(){
		this.ownerName = "";
		this.ID = Vehicle.nextID++;
    }

    public Vehicle(String ownerName){
        this.ownerName = ownerName;
		this.ID = Vehicle.nextID++;;
    }

    public static int maxID(){
        return nextID - 1;
    }

    @Override
    public String toString(){
        String str = speed + " " + angle + " " + ID;
        return str;
    }

    public static void main(String args[]){
        Vehicle v1 = new Vehicle();
        v1.speed = 55;
        v1.angle = 0;

        Vehicle v2 = new Vehicle();
        v2.speed = 80;
        v2.angle = 180;

        System.out.println(v1.toString());
        System.out.println(v2.toString());
    }

}