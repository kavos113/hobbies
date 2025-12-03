public class LinkedList {
    Object listItem;
    LinkedList nextItem;

    @Override
    public String toString(){
        return listItem + " " + nextItem;
    }

    public static void main(String args[]){
        Vehicle v1 = new Vehicle();
        v1.ID = Vehicle.nextID++;
        v1.speed = 55;
        v1.angle = 0;

        Vehicle v2 = new Vehicle();
        v2.ID = Vehicle.nextID++;
        v2.speed = 80;
        v2.angle = 180;

        LinkedList list1 = new LinkedList();
        list1.listItem = v1;

        LinkedList list2 = new LinkedList();
        list2.listItem = v2;

        list1.nextItem = list2;
    }
}