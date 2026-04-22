$javafx = "C:\Users\trecv\Downloads\openjfx-26_windows-x64_bin-sdk\javafx-sdk-26\lib"

$javaSources = Get-ChildItem -Path src -Recurse -Filter *.java |
    Where-Object { $_.Name -notlike "*Test*" } |
    Select-Object -ExpandProperty FullName

javac --module-path $javafx --add-modules javafx.controls,javafx.fxml -d src $javaSources

java --module-path $javafx --add-modules javafx.controls,javafx.fxml --enable-native-access=javafx.graphics -cp src app.App