public class Main {
    public static void main(String[] args) {
        BankAccount a = new BankAccount(100.0);
        a.deposit(3090);
        System.out.println("Balance: " + a.getBalance());
    }
}
