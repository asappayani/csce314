**App Purpose:**
This project's purpose is to be a simple American football scoreboard manager and display. In my app the user is allowed to:

    1. Set the home and away team names
    2. Update the scores of each team through the use of buttons
    3. Undo the last scoring action
    4. Clear the game scores to 0.

Its academic purpose is to give us hands on experience with brainstorming, planning, and then constructing our own program 
where we have to implement object oriented programming using an MVC design process.



**How it incorporates MVC Design:**
The model directory is where you can find the declaration of the Team class, TeamManager, and ScoreManager. This directory holds 
all the team data, scoring logic, input validation, and undo history. The model itself does not require or even need to know the 
existence of the view directory or controller directory to function properly.

The view directory is where the .fxml and .css files reside, and it handles the display layer of the application. 

Lastly, the controller directory holds the two controllers needed to connect the view and the model together. These controllers take 
these user inputs, updates the info inside the model, and then updates the view displays. The DisplayController wires the UI events '
for the scoreboard display, while the ScoreboardController wires the UI events for the controller display. 



**How to build/run the app:**
1. Open build-run.ps1 located in the root directory.
2. Update the $javafx variable (line 1) so that it is the path to YOUR javafx library
3. While in the root directory of the project, type "./build-run.ps1" into the terminal to run the powershell command.



**Model API Summary:**
Team.java defines the Team class. Each Team object holds the name and score of the team participating in the game. 
This class also holds the methods used to update the team name and their score.

TeamManager.java defines the Team Manager class. This class handles the updating and validating of team names.

ScoreManager.java defines the Score Manager class. This class applies score changes for a specific team. It also 
keeps track of the last action, meaning it's also in charge of undoing any scoring action. It also handles the clear 
feature, resetting all team scores to 0.

How to run tests:
Open your terminal, make sure you're in the root directory.
Type ".\test.ps1" and enter. 
This should automatically compile the files needed for ScoreboardTests.java and then run the unit tests.



**Known Limitations:**
The undo feature only keeps track of the last scoring action and not the full history of scoring actions.
The undo feature reverts the last action label to say "None" instead of telling us what decision was recently overturned.
On the scoreboard display, the quarter and game clock labels are static.










