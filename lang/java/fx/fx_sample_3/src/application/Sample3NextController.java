package application;

import java.io.IOException;

import javafx.event.Event;
import javafx.fxml.FXML;
import javafx.fxml.FXMLLoader;
import javafx.scene.Node;
import javafx.scene.Parent;
import javafx.scene.Scene;
import javafx.scene.control.Button;
import javafx.stage.Stage;
import javafx.stage.Window;

public class Sample3NextController {
	@FXML
	private Button BackChange_Button;

	@FXML
	public void BackChange_Button_onClick(Event eve) {
		Scene s = ((Node)eve.getSource()).getScene();
		Window window = s.getWindow();
		window.hide();

		try {
			Parent parent = FXMLLoader.load(getClass().getResource("Sample3.fxml"));
			Scene scene = new Scene(parent);
			Stage stage = new Stage();
			stage.setScene(scene);
			stage.setTitle("1kome");
			stage.show();
		}catch(IOException e) {
			e.printStackTrace();
		}
	}
}
