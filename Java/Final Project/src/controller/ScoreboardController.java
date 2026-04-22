package controller;

import javafx.event.ActionEvent;
import javafx.fxml.FXML;
import javafx.scene.control.Label;
import javafx.scene.control.TextField;
import javafx.scene.text.Text;
import model.ScoreManager;
import model.Team;
import model.TeamManager;

import javax.swing.JOptionPane;

public class ScoreboardController {

    @FXML
    private TextField awayNameField;

    @FXML
    private Label awayTeamNamePreview;

    @FXML
    private Text awayTeamScoreLabel;

    @FXML
    private TextField homeNameField;

    @FXML
    private Label homeTeamNamePreview;

    @FXML
    private Text homeTeamScoreLabel;

    @FXML
    private Label lastActionLabel;

    private Team homeTeam;
    private Team awayTeam;
    private TeamManager teamManager;
    private ScoreManager scoreManager;
    private DisplayController displayController;

    public void init(Team homeTeam, Team awayTeam, TeamManager teamManager,
                     ScoreManager scoreManager, DisplayController displayController) {
        this.homeTeam = homeTeam;
        this.awayTeam = awayTeam;
        this.teamManager = teamManager;
        this.scoreManager = scoreManager;
        this.displayController = displayController;
        refreshFromModel();
    }

    private void refreshFromModel() {
        homeTeamNamePreview.setText(safeName(homeTeam.getTeamName(), "HOME TEAM NAME"));
        awayTeamNamePreview.setText(safeName(awayTeam.getTeamName(), "AWAY TEAM NAME"));
        homeTeamScoreLabel.setText(String.valueOf(homeTeam.getScore()));
        awayTeamScoreLabel.setText(String.valueOf(awayTeam.getScore()));
        lastActionLabel.setText(scoreManager.displayLastAction());
        displayController.refreshFromModel();
    }

    private String safeName(String value, String fallback) {
        if (value == null || value.trim().isEmpty()) {
            return fallback;
        }
        return value;
    }

    private void showError(String message) {
    JOptionPane.showMessageDialog(
        null,
        message,
        "Scoreboard Error",
        JOptionPane.ERROR_MESSAGE
    );
}

    //thank god u showed us lambda functions b/c this made my code so much more readable instead of using a million try catches
    private void runAction(Runnable action) {
        try {
            action.run();
            refreshFromModel();
        } catch (IllegalStateException ex) {
            showError(ex.getMessage());
        } catch (Exception ex) {
            showError("Unexpected error: " + ex.getMessage());
        }
    }

    @FXML
    void awayConversionOrSafety(ActionEvent event) {
        runAction(() -> scoreManager.updateAwayScore(2, "PTS"));
    }

    @FXML
    void awayExtraPoint(ActionEvent event) {
        runAction(() -> scoreManager.updateAwayScore(1, "PAT"));
    }

    @FXML
    void awayFieldGoal(ActionEvent event) {
        runAction(() -> scoreManager.updateAwayScore(3, "FG"));
    }

    @FXML
    void awayTouchdown(ActionEvent event) {
        runAction(() -> scoreManager.updateAwayScore(6, "TD"));
    }

    @FXML
    void clearGame(ActionEvent event) {
        scoreManager.clear();
        refreshFromModel();
    }

    @FXML
    void homeConversionOrSafety(ActionEvent event) {
        runAction(() -> scoreManager.updateHomeScore(2, "PTS"));
    }

    @FXML
    void homeExtraPoint(ActionEvent event) {
        runAction(() -> scoreManager.updateHomeScore(1, "PAT"));
    }

    @FXML
    void homeFieldGoal(ActionEvent event) {
        runAction(() -> scoreManager.updateHomeScore(3, "FG"));
    }

    @FXML
    void homeTouchdown(ActionEvent event) {
        runAction(() -> scoreManager.updateHomeScore(6, "TD"));
    }

    @FXML
    void setAwayName(ActionEvent event) {
        teamManager.setAwayTeamName(awayNameField.getText());
        refreshFromModel();
    }

    @FXML
    void setHomeName(ActionEvent event) {
        teamManager.setHomeTeamName(homeNameField.getText());
        refreshFromModel();
    }

    @FXML
    void undoLastAction(ActionEvent event) {
        runAction(() -> scoreManager.undo());
        refreshFromModel();
    }
}
