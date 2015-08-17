package org.ninesyllables.scarletto;
import java.io.File;
import java.io.Writer;
import java.io.BufferedWriter;
import java.io.OutputStreamWriter;
import java.io.FileOutputStream;
public class DirUtils{
   // http://stackoverflow.com/questions/11113974/what-is-the-cross-platform-way-of-obtaining-the-path-to-the-local-application-da
  static String workingDirectory;
    //here, we assign the name of the OS, according to Java, to a variable...
  static String OS = (System.getProperty("os.name")).toUpperCase();
  public static String getResFolder(){

    //to determine what the workingDirectory is.
    //if it is some version of Windows
    if (OS.contains("WIN"))
    {
      //it is simply the location of the "AppData" folder
      workingDirectory = System.getenv("AppData");
    }
    //Otherwise, we assume Linux or Mac
    else
    {
      //in either case, we would start in the user's home directory
      workingDirectory = System.getProperty("user.home");
      //if we are on a Mac, we are not done, we look for "Application Support"
      workingDirectory += "/Library/Application Support";
    }
    return workingDirectory;
  }

  public static void prepare(){
    File f = new File(getResFolder() + "/CascadingRed/");
    File p = new File(getResFolder() + "/CascadingRed/config.toml");
    if(!p.exists()) {
      try{
        p.createNewFile();
      } catch (Exception e){
        System.out.println("Cannot create new file.");
      }
      try (Writer writer = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(getResFolder() + "/CascadingRed/config.toml"), "utf-8"))){
        writer.write("# CascadingRed 绯色无念 Configuration File");
        writer.write("\n");
        writer.write("[window]");
        writer.write("\n");
        writer.write("resolution = [960, 720]");
        writer.write("\n");
        writer.write("fullscreen = false");
      }
      catch (Exception e){
      }
    }
    f.mkdirs();
  }

  public static String getConfigPath(){
    return getResFolder() + "/CascadingRed/config.toml";
  }
}
