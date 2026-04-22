$javafx = "C:\Users\trecv\Downloads\openjfx-26_windows-x64_bin-sdk\javafx-sdk-26\lib"
$outDir = "out"

if (!(Test-Path $outDir)) {
    New-Item -ItemType Directory -Path $outDir | Out-Null
}

javac --module-path $javafx --add-modules javafx.controls,javafx.fxml -d $outDir `
    src\controller\ScoreboardController.java `
    src\model\*.java `
    src\app\App.java

if ($LASTEXITCODE -ne 0) {
    exit $LASTEXITCODE
}

java --module-path $javafx --add-modules javafx.controls,javafx.fxml -cp $outDir app.App