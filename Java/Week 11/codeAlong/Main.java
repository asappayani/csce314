import java.util.List;

public class Main {
    public static void main(String[] args) {
        Library lib = new Library();

        LibraryItem hb = new Hardback("Effective Java", "Joshua Bloch", 416);
        LibraryItem pb = new Paperback("Clean Code", "Robert C. Martin", 464);
        LibraryItem mag = new Periodical("ACM Communications", "Oct 2025");

        lib.addItem(hb); 
        lib.addItem(pb); 
        lib.addItem(mag);

        Patron alice = new Patron("Alice");
        Patron bob = new Patron("Bob");

        lib.addPatron(alice); 
        lib.addPatron(bob);

        lib.printInventory(); 
        System.out.println();

        System.out.println("=== Interface Demo (Checkable) ===");
        for (Checkable c : List.of(hb, pb, mag)) {
            System.out.println(((LibraryItem)c).getTitle() + " checked out? " + c.isCheckedOut());
        }
        System.out.println();

        boolean a1 = lib.checkout(alice.getId(), hb.getId());
        boolean a2 = lib.checkout(alice.getId(), pb.getId());
        System.out.printf("Alice borrow results: %b, %b%n", a1, a2);

        boolean b1 = lib.checkout(bob.getId(), mag.getId());
        boolean b2 = lib.checkout(bob.getId(), pb.getId()); // should fail
        System.out.printf("Bob borrow results: %b, %b%n", b1, b2);
        System.out.println();

        lib.printInventory(); 
        System.out.println();
        lib.printPatrons(); 
        System.out.println();

        System.out.println("=== Returns and Re-Checkouts ===");
        boolean r1 = lib.checkin(pb.getId());              // Alice returns paperback
        boolean r2 = lib.checkout(bob.getId(), pb.getId()); // Bob borrows it
        System.out.printf("Return then borrow: %b, %b%n", r1, r2);
        System.out.println();

        lib.printInventory(); 
        System.out.println();
        lib.printPatrons();
    }
}
