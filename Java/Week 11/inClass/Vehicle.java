public abstract class Vehicle implements Operable {
    private static int NEXT_ID = 1000; // auto-increment
    private final int id;
    private final String make;
    private final String model;
    private final int year;

    protected Vehicle(String make, String model, int year) {
        this.id = NEXT_ID++;
        this.make = make;
        this.model = model;
        this.year = year;
    }

    public int getId() { return id; }
    public String getMake() { return make; }
    public String getModel(){ return model; }
    public int getYear() { return year; }

    @Override
    public String toString() {
        return String.format("%s #%d: %d %s %s", 
        this.getClass().getSimpleName(), id, year, make, model);
    }
}