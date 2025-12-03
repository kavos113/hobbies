package fxtest;

import javafx.application.Application;
import javafx.geometry.Pos;
import javafx.scene.Scene;
import javafx.scene.layout.FlowPane;
import javafx.scene.text.Font;
import javafx.scene.text.Text;
import javafx.stage.Stage;

public class FxTest1 extends Application {

	public static void main (String args[]) {
		launch(args);
	}

	@Override
	public void start(Stage primaryStage) throws Exception {
		Text text = new Text();
		text.setText("2048ああああ");
		text.setFont(Font.font("MS Mincho", 40));

		FlowPane pane = new FlowPane();
		pane.setStyle("-fx-background-color: #b2dfdb");
		pane.setAlignment(Pos.CENTER);
		pane.getChildren().add(text);

		Scene scene = new Scene(pane,300,150);
		primaryStage.setScene(scene);
		primaryStage.setTitle("title");
		primaryStage.show();
	}

}
