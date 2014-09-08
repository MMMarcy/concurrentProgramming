import TSim.CommandException;
import TSim.SensorEvent;
import TSim.TSimInterface;

import java.util.*;
import java.util.concurrent.Executor;
import java.util.concurrent.Executors;
import java.util.concurrent.Semaphore;

public class Lab1 {

    int simSpeed;
    public enum DIRECTION {
        down, up, noDirection
    }


    public static void main(String[] args) {
        new Lab1(args);
    }

    public static Collection<CS> initCSs() {
        return null;
    }

    public Lab1(String[] args) {


        TSimInterface tsi = TSimInterface.getInstance();
        int train1Speed = Integer.parseInt(args[0]);
        int train2speed = Integer.parseInt(args[1]);
        simSpeed = Integer.parseInt(args[2]);


        //TODO: Add Direction when adding CS to switch to be able to match the CS in a unique manner


        CS csOne = new CS(1, true, "one");
        CS csTwo = new CS(1, true, "two");
        CS csThree = new CS(1, true, "three");
        CS csFour = new CS(1, true, "four");
        CS csFive = new CS(1, true, "five");
        CS csSix = new CS(1, true, "six");
        CS csSeven = new CS(1, true, "seven");
        CS csEight = new CS(1, true, "eight");
        CS csNine = new CS(1, true, "nine");


        Switch one = new Switch(tsi);
        one.addCS(csOne, DIRECTION.down);
        one.addCS(csTwo, DIRECTION.down);
        one.addCS(csThree, DIRECTION.up);
        one.addSensor(new Sensor(3, 12));
        one.addSensor(new Sensor(4, 11));
        one.addSensor(new Sensor(2, 11));

        Switch two = new Switch(tsi);
        two.addCS(csThree, DIRECTION.down);
        two.addCS(csFour, DIRECTION.up);
        two.addCS(csFive, DIRECTION.up);
        two.addSensor(new Sensor(3, 9));
        two.addSensor(new Sensor(4, 10));
        two.addSensor(new Sensor(5, 9));


        Switch three = new Switch(tsi);
        three.addCS(csFour, DIRECTION.down);
        three.addCS(csFive, DIRECTION.down);
        three.addCS(csSix, DIRECTION.up);
        three.addSensor(new Sensor(14, 9));
        three.addSensor(new Sensor(15, 10));
        three.addSensor(new Sensor(16, 9));

        Switch four = new Switch(tsi);
        four.addCS(csSix, DIRECTION.down);
        four.addCS(csSeven, DIRECTION.up);
        four.addCS(csEight, DIRECTION.up);
        four.addSensor(new Sensor(18, 7));
        four.addSensor(new Sensor(15, 10));
        four.addSensor(new Sensor(16, 7));


        Switch five = new Switch(tsi);
        five.addCS(csNine, DIRECTION.noDirection);
        five.addSensor(new Sensor(8, 8));
        five.addSensor(new Sensor(7, 7));
        five.addSensor(new Sensor(9, 7));
        five.addSensor(new Sensor(8, 6));

        HashSet<Switch> switches = new HashSet<Switch>();
        switches.add(one);
        switches.add(two);
        switches.add(three);
        switches.add(four);
        switches.add(five);

        Train t1 = new Train(1, train1Speed, tsi, switches);
        Train t2 = new Train(2, train2speed, tsi, switches);
        Executor executor = Executors.newFixedThreadPool(2);
        executor.execute(t1);
        executor.execute(t2);

    }

    class Train implements Runnable {

        private Integer trainId;
        private TSimInterface tsi;
        private Integer initialSpeed;
        private HashSet<Switch> switches;



        public DIRECTION getDirection() {
            if (this.trainId == 1) {
                if (this.initialSpeed < 0) {
                    return DIRECTION.up;
                } else {
                    return DIRECTION.down;
                }
            } else {
                if (this.initialSpeed < 0) {
                    return DIRECTION.down;
                } else {
                    return DIRECTION.up;
                }
            }
        }


        public Train(int trainId, int initialSpeed, TSimInterface tsi, HashSet<Switch> switches) {
            this.trainId = trainId;
            this.initialSpeed = initialSpeed;
            this.tsi = tsi;
            this.switches = switches;
        }

        @Override
        public void run() {
            try {
                tsi.setSpeed(trainId, initialSpeed);

                while (true) {

                    SensorEvent e = tsi.getSensor(trainId);
                    if(e.getStatus() == SensorEvent.INACTIVE)
                        continue;

                    for (Switch s : switches) {
                        for(Sensor sen : s.sensors) {

                            if(sen.equals(e)) {
                                DIRECTION trainDirection = getDirection();
                                for(CS cs: s.getCS().keySet()){
                                   DIRECTION csDirection = s.getCS().get(cs);
                                    //Acquiring
                                    if(trainDirection == csDirection){
                                        if(cs.availablePermits() == 0){
                                            if(cs.occupiedByTrainId != trainId ){
                                                System.err.println("Acquiring lock");
                                                tsi.setSpeed(trainId, 0);
                                                cs.acquire(trainId);
                                                tsi.setSpeed(trainId, initialSpeed);
                                            }
                                        } else {
                                            cs.acquire(trainId);
                                        }
                                    }
                                    //Releasing
                                    else {
                                        if(cs.availablePermits() == 0 && cs.occupiedByTrainId == trainId){
                                            System.err.println("Releasing lock");
                                            cs.release();
                                        }
                                    }
                                }
                            }

                        }
                    }


                }
            } catch (CommandException e) {
                e.printStackTrace();
            } catch (InterruptedException e) {
                e.printStackTrace();
            }
        }
    }


    class Sensor {
        int x;
        int y;

        public Sensor(int x, int y) {
            this.x = x;
            this.y = y;
        }

        @Override
        public boolean equals(Object obj) {
            SensorEvent s = (SensorEvent) obj;
            return this.x == s.getXpos() && this.y == s.getYpos();
        }

        @Override
        public String toString() {
            return "Sensor: "+x+" "+y;
        }
    }


    class Switch {

        TSimInterface tsi;
        Map<CS, DIRECTION> cssWithDirection;
        HashSet<Sensor> sensors;

        public Switch(TSimInterface tsi) {
            this.tsi = tsi;
            this.cssWithDirection = new HashMap<CS, DIRECTION>();
            this.sensors = new HashSet<Sensor>();
        }

        public void addCS(CS cs, DIRECTION dir) {
            cssWithDirection.put(cs, dir);
        }

        public void addSensor(Sensor s){
            sensors.add(s);
        }

        public Map<CS, DIRECTION> getCS() {
            return this.cssWithDirection;
        }
    }

    class CS extends Semaphore {
        String label;
        int occupiedByTrainId;


        public CS(int permits, String label) {
            super(permits);
            this.label = label;
        }

        public CS(int permits, boolean fair, String label) {
            super(permits, fair);
            this.label = label;
        }


        public void acquire(int trainId) throws InterruptedException {
            System.err.println("CS "+label+" acquired by "+trainId);
            super.acquire();
            this.occupiedByTrainId = trainId;
        }

        @Override
        public void release() {
            System.err.println("CS "+label+" released by "+occupiedByTrainId);
            super.release();
            occupiedByTrainId = -1;
        }
    }

}


