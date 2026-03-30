public class Tractor extends Vehicle {
    private int liftCap;
    
    public Tractor(String make, String model, int year, int liftCap){
        super(make, model, year);
        this.liftCap = liftCap;
    }

    @Override
    public void start() { System.out.println("Tractor" + getId() + " engine on."); }

    @Override
    public void stop() { System.out.println("Tractor " + getId() + " engine off."); }

    @Override
    public void accelerate(int amount) {
        System.out.println("Tractor " + getId() + " accelerates by " + amount + " mph.");
    }

    @Override
    public String toString() {
        return String.format("%s, liftCap=%d", super.toString(), liftCap);
    }
}