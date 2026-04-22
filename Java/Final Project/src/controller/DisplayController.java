package controller;

import javafx.fxml.FXML;
import javafx.scene.control.Label;
import javafx.scene.text.Text;
import model.ScoreManager;
import model.Team;

public class DisplayController {

    @FXML
    private Label awayTeamLabel;

    @FXML
    private Text awayTeamScoreLabel;

    @FXML
    private Text currentGameTimeLabel;

    @FXML
    private Label currentQuarterLabel;

    @FXML
    private Label homeTeamLabel;

    @FXML
    private Text homeTeamScoreLabel;

    @FXML
    private Label lastActionLabel;

    private Team homeTeam;
    private Team awayTeam;
    private ScoreManager scoreManager;

    public void init(Team homeTeam, Team awayTeam, ScoreManager scoreManager) {
        this.homeTeam = homeTeam;
        this.awayTeam = awayTeam;
        this.scoreManager = scoreManager;
        refreshFromModel();
    }

    public void refreshFromModel() {
        homeTeamLabel.setText(safeName(homeTeam.getTeamName(), "HOME TEAM NAME"));
        awayTeamLabel.setText(safeName(awayTeam.getTeamName(), "AWAY TEAM NAME"));
        homeTeamScoreLabel.setText(String.valueOf(homeTeam.getScore()));
        awayTeamScoreLabel.setText(String.valueOf(awayTeam.getScore()));
        lastActionLabel.setText(scoreManager.displayLastAction());

        currentQuarterLabel.setText("1ST QUARTER");
        currentGameTimeLabel.setText("15:00");
    }

    private String safeName(String value, String fallback) {
        if (value == null || value.trim().isEmpty()) {
            return fallback;
        }
        return value;
    }
}
