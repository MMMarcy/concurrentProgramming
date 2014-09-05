import TSim.CommandException;
import TSim.SensorEvent;
import TSim.TSimInformation;
import TSim.TSimInterface;

import java.util.Collection;
import java.util.concurrent.Executor;
import java.util.concurrent.Executors;
import java.util.concurrent.Semaphore;
import java.util.concurrent.TimeUnit;

public class Lab1 {

    int simSpeed;

    public static void main(String[] args) {
        new Lab1(args);
    }

    public Lab1(String[] args) {

        TSimInterface tsi = TSimInterface.getInstance();


        int train1Speed = Integer.parseInt(args[0]);
        int train2speed = Integer.parseInt(args[1]);
        simSpeed = Integer.parseInt(args[2]);

        Train t1 = new Train(1, train1Speed, tsi);
        Train t2 = new Train(2, train2speed, tsi);
        Executor executor = Executors.newFixedThreadPool(2);
        executor.execute(t1);
        executor.execute(t2);

    }

    class Train implements Runnable {

        private Integer trainId;
        private TSimInterface tsi;
        private Integer initialSpeed;


        public Train(int trainId, int initialSpeed, TSimInterface tsi) {
            this.trainId = trainId;
            this.initialSpeed = initialSpeed;
            this.tsi = tsi;
        }

        @Override
        public void run() {
            try {
                tsi.setSpeed(trainId, initialSpeed);

                while (true) {

                    SensorEvent e = tsi.getSensor(trainId);
                    System.err.println(e);
                    initialSpeed *= -1;

                    tsi.setSpeed(trainId, 0);

                    TimeUnit.MILLISECONDS.sleep(3 * simSpeed * Math.abs(initialSpeed));

                    tsi.setSpeed(trainId, initialSpeed);

                }
            } catch (CommandException e) {
                e.printStackTrace();
            } catch (InterruptedException e) {
                e.printStackTrace();
            }
        }
    }


    class Sensor{
        int x;
        int y;

        public Sensor(int x, int y){
            this.x = x;
            this.y = y;
        }
    }


    class CS{
        private Collection<Sensor> sensors;
        Semaphore semaphore = new Semaphore(1, true);

        public  CS(Collection<Sensor> sensors){
            this.sensors = sensors;
        }




    }




}


