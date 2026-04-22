package app;

import javafx.application.Application;
import javafx.fxml.FXMLLoader;
import javafx.scene.Scene;
import javafx.stage.Stage;

public class App extends Application {

    @Override
    public void start(Stage primaryStage) throws Exception {
        // Load the scoreboard manager window (main/primary window)
        FXMLLoader managerLoader = new FXMLLoader(getClass().getResource("/view/scoreboardmanager.fxml"));
        Scene managerScene = new Scene(managerLoader.load());
        
        primaryStage.setTitle("Scoreboard Manager");
        primaryStage.setScene(managerScene);
        primaryStage.show();

        // Load the scoreboard display window (secondary window)
        FXMLLoader displayLoader = new FXMLLoader(getClass().getResource("/view/scoreboarddisplay.fxml"));
        Scene displayScene = new Scene(displayLoader.load());
        
        Stage displayStage = new Stage();
        displayStage.setTitle("Scoreboard Display");
        displayStage.setScene(displayScene);
        displayStage.show();
    }

    public static void main(String[] args) {
        launch(args);
    }
}