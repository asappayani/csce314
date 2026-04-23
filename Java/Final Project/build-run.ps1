$javafx = "C:\Users\trecv\Downloads\openjfx-26_windows-x64_bin-sdk\javafx-sdk-26\lib"

javac --module-path $javafx --add-modules javafx.controls,javafx.fxml -d src `
	src\app\App.java `
	src\controller\DisplayController.java `
	src\controller\ScoreboardController.java `
	src\model\ScoreManager.java `
	src\model\Team.java `
	src\model\TeamManager.java

java --module-path $javafx --add-modules javafx.controls,javafx.fxml --enable-native-access=javafx.graphics -cp src app.App