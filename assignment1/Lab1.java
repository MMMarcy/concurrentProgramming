import TSim.*;

public class Lab1 {

  public static void main(String[] args) {
    new Lab1(args);
  }

  public Lab1(String[] args) {
    TSimInterface tsi = TSimInterface.getInstance();

    try {
      tsi.setSpeed(1,10);
    }
    catch (CommandException e) {
      e.printStackTrace();    // or only e.getMessage() for the error
      System.exit(1);
    }
  }
}
