import javax.swing.JOptionPane;

public class Main {
    public static void main(String[] args) {
        JOptionPane.showMessageDialog(null, "Howdy! Welcome to this random program.");
        
        Name name = new Name(JOptionPane.showInputDialog("Enter your name:"));

        if (!(name.getName().equals(""))) {
            Color colorClass = new Color();
            String[] colors = colorClass.getColors();

            String favColor = (String) JOptionPane.showInputDialog(
                null,
                "Hi " + name.getName() +", choose your favorite color:",
                "Color Choice",
                JOptionPane.QUESTION_MESSAGE,
                null,
                colors,
                colors[0]
            );

            JOptionPane.showMessageDialog(null, "Hi " + name.getName() + ", your favorite color is " + favColor + "!");
        } else {
            JOptionPane.showMessageDialog(null, "You didn't input a name.");
        }

        
        

    }
}
