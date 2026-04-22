package app;

import controller.DisplayController;
import controller.ScoreboardController;
import javafx.application.Application;
import javafx.fxml.FXMLLoader;
import javafx.scene.Scene;
import javafx.stage.Stage;
import model.ScoreManager;
import model.Team;
import model.TeamManager;

public class App extends Application {

    @Override
    public void start(Stage primaryStage) throws Exception {

        Team home = new Team();
        Team away = new Team();
        TeamManager teamManager = new TeamManager(home, away);
        ScoreManager scoreManager = new ScoreManager(home, away);

        FXMLLoader displayLoader = new FXMLLoader(getClass().getResource("/view/scoreboarddisplay.fxml"));
        Scene displayScene = new Scene(displayLoader.load());
        DisplayController displayController = displayLoader.getController();

        FXMLLoader managerLoader = new FXMLLoader(getClass().getResource("/view/scoreboardmanager.fxml"));
        Scene managerScene = new Scene(managerLoader.load());
        ScoreboardController managerController = managerLoader.getController();

        displayController.init(home, away, scoreManager);
        managerController.init(home, away, teamManager, scoreManager, displayController);

        primaryStage.setTitle("Scoreboard Manager");
        primaryStage.setScene(managerScene);
        primaryStage.show();

        Stage displayStage = new Stage();
        displayStage.setTitle("Scoreboard Display");
        displayStage.setScene(displayScene);
        displayStage.show();
    }

    public static void main(String[] args) {
        launch(args);
    }
}