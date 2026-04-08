public class Patron {
    private static int NEXT_ID = 1;
    private static final int MAX_LOANS = 3;

    private final int id;
    private final String name;
    private final java.util.List<LibraryItem> loans = new java.util.ArrayList<>();

    private boolean eligibleForCheckout = true;

    public Patron(String name) { this.id = NEXT_ID++; this.name = name; }
    public int getId() { return id; }
    public String getName() { return name; }
    public java.util.List<LibraryItem> getLoans() { return loans; }
    public boolean canBorrow() { return loans.size() < MAX_LOANS; }

    public boolean borrow(LibraryItem item) {
        if (!canBorrow()) return false;
        if (!isEligibleForCheckout()) return false;
        if (item.checkOut(this)) { loans.add(item); return true; }
        return false;
    }

    public boolean returnItem(LibraryItem item) {
        if (loans.contains(item) && item.checkIn()) { loans.remove(item); return true; }
        return false;
    }

    @Override public String toString() {
        return String.format("Patron #%d %s (loans: %d)", id, name, loans.size());
    }

    public boolean isEligibleForCheckout(){
        return eligibleForCheckout;
    }

    public void setEligibleForCheckout(boolean eligible) {
        this.eligibleForCheckout = eligible;
    }
}
