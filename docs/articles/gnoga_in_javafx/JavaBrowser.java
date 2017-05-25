import javafx.application.Application;
import javafx.scene.Scene;
import javafx.scene.web.WebEngine;
import javafx.scene.web.WebView;
import javafx.stage.Stage;
import java.util.*;
 
 
public class JavaBrowser extends Application {
    private Scene scene;
    @Override public void start(Stage stage) {

        int width  = 750;
        int height = 500;
        String url = "http://127.0.0.1:8080";
        String title = "";

        List<String> params = getParameters().getRaw();

        if(params.size() > 0){
           url = params.get(0);
        }

        if(params.size() > 1){
           try{
              width = Integer.parseInt(params.get(1));
           }catch(NumberFormatException e){
              System.out.println("Invalid Width Specified");
           }
        }

        if(params.size() > 2){
           try{
              height = Integer.parseInt(params.get(2));
           }catch(NumberFormatException e){
              System.out.println("Invalid Height Specified");
           }
        }

        if(params.size() > 3){
           title = params.get(3);
           for (int i = 4; i < params.size(); i++){
              title = title + " " + params.get(i);
           }
        }

        


        WebView browser = new WebView();
        
        // create the scene
        stage.setTitle(title);
        scene = new Scene(browser,width,height);
        stage.setScene(scene);     
        stage.show();

        WebEngine webEngine = browser.getEngine();
        webEngine.load(url);
    }
 
    public static void main(String[] args){
        launch(args);
    }
}