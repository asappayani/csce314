/**
 * Simple immutable Contact data type.
 * You may extend this class if you want more fields.
 */
public final class Contact {
    private final String name;
    private final String phone;

    public Contact(String name, String phone) {
        // TODO:
        // 1. Check whether name is null or blank.
        // 2. Check whether phone is null or blank.
        // 3. If either is invalid, throw an IllegalArgumentException
        //    with a helpful message.
        // 4. Otherwise, store trimmed versions of name and phone.
        if (name == null || phone == null || name.trim().isEmpty() || phone.trim().isEmpty()) {
            throw new IllegalArgumentException("Your name and phone number cannot be blank.");
        }

        this.name = name.trim();
        this.phone = phone.trim();
    }

    public String getName() {
        // TODO: return the contact's name
        return this.name;
    }

    public String getPhone() {
        // TODO: return the contact's phone number
        return this.phone;
    }

    @Override
    public String toString() {
        // TODO: return output in this format:
        // name + " -> " + phone
        return name + " -> " + phone;
    }
}
