import TSim.CommandException;
import TSim.SensorEvent;
import TSim.TSimInterface;

import java.util.concurrent.Executor;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

public class Lab1 {

    public static void main(String[] args) {
        new Lab1(args);
    }

    public Lab1(String[] args) {
        TSimInterface tsi = TSimInterface.getInstance();

        try {

            tsi.setSpeed(1, 20);
            tsi.setSpeed(2, 80);
            tsi.setSwitch(15, 9, TSimInterface.SWITCH_RIGHT);
            Train t = new Train(2, tsi);
            Executor executor = Executors.newFixedThreadPool(2);
            executor.execute(t);

        } catch (CommandException e) {
            e.printStackTrace();    // or only e.getMessage() for the error
            System.exit(1);
        }
    }
}

class Train implements Runnable {

    private Integer trainId;
    private TSimInterface tsi;

    public Train(int trainId, TSimInterface tsi) {
        this.trainId = trainId;
        this.tsi = tsi;
    }

    @Override
    public void run() {
        while (true) {
            try {
                SensorEvent e = tsi.getSensor(trainId);
                tsi.setSpeed(trainId, 0);
            } catch (CommandException e) {
                e.printStackTrace();
            } catch (InterruptedException e) {
                e.printStackTrace();
            }
        }
    }
}
