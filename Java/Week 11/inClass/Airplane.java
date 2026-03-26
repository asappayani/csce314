public class Airplane extends Vehicle {
    private int numDoors;
    
    public Airplane(String make, String model, int year, int numDoors){
        super(make, model, year);
        this.numDoors = numDoors;
    }

    @Override
    public void start() { System.out.println("Airplane " + getId() + " engine on."); }

    @Override
    public void stop() { System.out.println("Airplane " + getId() + " engine off."); }

    @Override
    public void accelerate(int amount) {
        System.out.println("Airplane " + getId() + " accelerates by " + amount + " mph.");
    }
}