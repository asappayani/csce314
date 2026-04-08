import java.io.File;
import java.io.FileNotFoundException;
import java.util.Scanner;

public class Main {
    public static Library loadLibraryFromFiles(String patron_file, String item_file){
        Library lib = new Library();

        try (Scanner sc = new Scanner(new File(patron_file))) {
            int lineNum = 0;
            while (sc.hasNextLine()) {
                lineNum++;
                String line = sc.nextLine().trim();
                if (line.isEmpty()) continue;

                String[] parts = line.split(",", 2);
                if (parts.length != 2) {
                    System.out.println("Warning: malformed patron line " + lineNum + ": " + line);
                    continue;
                }

                String name = parts[0].trim();
                String flag = parts[1].trim();

                if (name.isEmpty()) {
                    System.out.println("Warning: empty patron name on line " + lineNum);
                    continue;
                }

                Patron p = new Patron(name);
                p.setEligibleForCheckout(flag.equalsIgnoreCase("y"));
                lib.addPatron(p);
            }
        } catch (FileNotFoundException e) {
            throw new RuntimeException("Could not open patron file: " + patron_file, e);
        }

        try (Scanner sc = new Scanner(new File(item_file))) {
            int lineNum = 0;
            while (sc.hasNextLine()) {
                lineNum++;
                String line = sc.nextLine().trim();
                if (line.isEmpty()) continue;
        
                String[] parts = line.split(",");
                for (int i = 0; i < parts.length; i++) {
                    parts[i] = parts[i].trim();
                }
        
                String type = parts[0].toUpperCase();
        
                try {
                    switch (type) {
                        case "HARDBACK":
                            if (parts.length != 4) {
                                System.out.println("Warning: malformed HARDBACK line " + lineNum + ": " + line);
                                continue;
                            }
                            lib.addItem(new Hardback(parts[1], parts[2], Integer.parseInt(parts[3])));
                            break;
        
                        case "PAPERBACK":
                            if (parts.length != 4) {
                                System.out.println("Warning: malformed PAPERBACK line " + lineNum + ": " + line);
                                continue;
                            }
                            lib.addItem(new Paperback(parts[1], parts[2], Integer.parseInt(parts[3])));
                            break;
        
                        case "PERIODICAL":
                            if (parts.length != 3) {
                                System.out.println("Warning: malformed PERIODICAL line " + lineNum + ": " + line);
                                continue;
                            }
                            lib.addItem(new Periodical(parts[1], parts[2]));
                            break;
        
                        case "BOOK_ON_TAPE":
                            if (parts.length != 4) {
                                System.out.println("Warning: malformed BOOK_ON_TAPE line " + lineNum + ": " + line);
                                continue;
                            }
                            lib.addItem(new BookOnTape(parts[1], parts[2], Integer.parseInt(parts[3])));
                            break;
        
                        case "VIDEO":
                            if (parts.length != 4) {
                                System.out.println("Warning: malformed VIDEO line " + lineNum + ": " + line);
                                continue;
                            }
                            lib.addItem(new VideoMedium(parts[1], Integer.parseInt(parts[2]), parts[3]));
                            break;
        
                        default:
                            System.out.println("Warning: unknown item type on line " + lineNum + ": " + line);
                    }
                } catch (NumberFormatException e) {
                    System.out.println("Warning: bad numeric value on item line " + lineNum + ": " + line);
                }
            }
        } catch (FileNotFoundException e) {
            throw new RuntimeException("Could not open item file: " + item_file, e);
        }        
        return lib;
    }
    public static void main(String[] args) {
        Scanner in = new Scanner(System.in);

        String patronFile;
        String itemFile;

        if (args.length >= 2) {
            patronFile = args[0];
            itemFile = args[1];
        } else {
            System.out.print("Enter patron file path: ");
            patronFile = in.nextLine().trim();
            System.out.print("Enter item file path: ");
            itemFile = in.nextLine().trim();
        }

        Library lib = loadLibraryFromFiles(patronFile, itemFile);

        while (true) {
            System.out.println("=== Library Menu ===");
            System.out.println("1) Inventory");
            System.out.println("2) Interface Demo");
            System.out.println("3) Patrons");
            System.out.println("4) Checkout");
            System.out.println("5) Checkin");
            System.out.println("6) Toggle Patron Eligibility");
            System.out.println("7) Quit");
            System.out.print("Choice: ");

            int choice;
            try {
                choice = Integer.parseInt(in.nextLine().trim());
            } catch (Exception e) {
                System.out.println("Invalid choice.");
                System.out.println();
                continue;
            }

            if (choice == 1) {
                for (LibraryItem item : lib.getItems()) {
                    System.out.println("[" + item.getId() + "] " + item.getClass().getSimpleName()
                            + " - \"" + item.getTitle() + "\" (checkedOut=" + item.isCheckedOut() + ")");
                }
            } else if (choice == 2) {
                for (LibraryItem item : lib.getItems()) {
                    Checkable c = item;
                    System.out.println(item.getTitle() + ": checkedOut=" + c.isCheckedOut()
                            + ", loanDays=" + item.getLoanPeriodDays());
                }
            } else if (choice == 3) {
                for (Patron p : lib.getPatrons()) {
                    System.out.println("[" + p.getId() + "] " + p.getName()
                            + " (eligible=" + p.isEligibleForCheckout()
                            + ", loans=" + p.getLoans().size() + ")");
                }
            } else if (choice == 4) {
                int patronId;
                int itemId;
                try {
                    System.out.print("Enter patron id: ");
                    patronId = Integer.parseInt(in.nextLine().trim());
                    System.out.print("Enter item id: ");
                    itemId = Integer.parseInt(in.nextLine().trim());
                } catch (Exception e) {
                    System.out.println("Checkout failed: invalid numeric input.");
                    System.out.println();
                    continue;
                }

                var pOpt = lib.findPatron(patronId);
                var iOpt = lib.findItem(itemId);

                if (pOpt.isEmpty()) {
                    System.out.println("Checkout failed: patron id not found.");
                } else if (iOpt.isEmpty()) {
                    System.out.println("Checkout failed: item id not found.");
                } else {
                    Patron p = pOpt.get();
                    LibraryItem item = iOpt.get();

                    if (!p.isEligibleForCheckout()) {
                        System.out.println("Checkout failed: Patron not eligible to checkout.");
                    } else if (!p.canBorrow()) {
                        System.out.println("Checkout failed: Patron has max loans.");
                    } else if (item.isCheckedOut()) {
                        System.out.println("Checkout failed: Item already checked out.");
                    } else {
                        boolean ok = lib.checkout(patronId, itemId);
                        if (ok) System.out.println("Checkout success: " + item.getTitle() + " -> Patron " + p.getId());
                        else System.out.println("Checkout failed.");
                    }
                }
            } else if (choice == 5) {
                int itemId;
                try {
                    System.out.print("Enter item id: ");
                    itemId = Integer.parseInt(in.nextLine().trim());
                } catch (Exception e) {
                    System.out.println("Checkin failed: invalid numeric input.");
                    System.out.println();
                    continue;
                }

                var iOpt = lib.findItem(itemId);
                if (iOpt.isEmpty()) {
                    System.out.println("Checkin failed: item id not found.");
                } else if (!iOpt.get().isCheckedOut()) {
                    System.out.println("Checkin failed: item is not currently checked out.");
                } else {
                    boolean ok = lib.checkin(itemId);
                    System.out.println(ok ? "Checkin success." : "Checkin failed.");
                }
            } else if (choice == 6) {
                int patronId;
                try {
                    System.out.print("Enter patron id: ");
                    patronId = Integer.parseInt(in.nextLine().trim());
                } catch (Exception e) {
                    System.out.println("Toggle failed: invalid numeric input.");
                    System.out.println();
                    continue;
                }

                var pOpt = lib.findPatron(patronId);
                if (pOpt.isEmpty()) {
                    System.out.println("Toggle failed: patron id not found.");
                } else {
                    Patron p = pOpt.get();
                    p.setEligibleForCheckout(!p.isEligibleForCheckout());
                    System.out.println("Patron eligibility toggled: now eligible=" + p.isEligibleForCheckout());
                }
            } else if (choice == 7) {
                System.out.println("Goodbye!");
                return;
            } else {
                System.out.println("Invalid choice.");
            }

            System.out.println();
        }
    }
}
