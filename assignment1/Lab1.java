import TSim.SensorEvent;
import TSim.TSimInterface;

import java.util.*;
import java.util.concurrent.Executor;
import java.util.concurrent.Executors;
import java.util.concurrent.Semaphore;
import java.util.concurrent.TimeUnit;

public class Lab1 {

    int simSpeed;

    public enum DIRECTION {
        down, up, noDirection
    }

    public static void main(String[] args) {
        try {
            new Lab1(args);
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    public Lab1(String[] args) throws Exception {

        TSimInterface tsi = TSimInterface.getInstance();
        int train1Speed = Integer.parseInt(args[0]);
        int train2speed = Integer.parseInt(args[1]);
        simSpeed = Integer.parseInt(args[2]);

        CS csOne = new CS(1, true, "one");
        CS csTwo = new CS(1, true, "two");
        csTwo.acquire(2);
        CS csThree = new CS(1, true, "three");
        CS csFour = new CS(1, true, "four");
        CS csFive = new CS(1, true, "five");
        CS csSix = new CS(1, true, "six");
        CS csSeven = new CS(1, true, "seven");
        CS csEight = new CS(1, true, "eight");
        csEight.acquire(1);
        CS csNine = new CS(1, true, "nine");

        Switch one = new Switch(tsi, 3, 11);
        one.addCS(csOne, DIRECTION.down);
        one.addCS(csTwo, DIRECTION.down);
        one.addCS(csThree, DIRECTION.up);
        one.addSensor(new Sensor(4, 13, DIRECTION.down));
        one.addSensor(new Sensor(6, 11, DIRECTION.down));
        one.addSensor(new Sensor(1, 11, DIRECTION.up));

        Switch two = new Switch(tsi, 4, 9);
        two.addCS(csThree, DIRECTION.down);
        two.addCS(csFive, DIRECTION.up);
        two.addCS(csFour, DIRECTION.up);
        two.addSensor(new Sensor(1, 10, DIRECTION.down));
        two.addSensor(new Sensor(7, 10, DIRECTION.up));
        two.addSensor(new Sensor(8, 9, DIRECTION.up));

        Switch three = new Switch(tsi, 15, 9);
        three.addCS(csFour, DIRECTION.down);
        three.addCS(csFive, DIRECTION.down);
        three.addCS(csSix, DIRECTION.up);
        three.addSensor(new Sensor(12, 9, DIRECTION.down));
        three.addSensor(new Sensor(13, 10, DIRECTION.down));
        three.addSensor(new Sensor(18, 9, DIRECTION.up));

        Switch four = new Switch(tsi, 17, 7);
        four.addCS(csSix, DIRECTION.down);
        four.addCS(csSeven, DIRECTION.up);
        four.addCS(csEight, DIRECTION.up);
        four.addSensor(new Sensor(19, 8, DIRECTION.down));
        four.addSensor(new Sensor(15, 8, DIRECTION.up));
        four.addSensor(new Sensor(14, 7, DIRECTION.up));


        Switch five = new Switch(tsi, -1, -1);
        five.addCS(csNine, DIRECTION.noDirection);
        five.addSensor(new Sensor(10, 8, DIRECTION.noDirection));
        five.addSensor(new Sensor(6, 6, DIRECTION.noDirection));
        five.addSensor(new Sensor(11, 7, DIRECTION.noDirection));
        five.addSensor(new Sensor(10, 5, DIRECTION.noDirection));

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
        private Sensor stationSensor1 = new Sensor(14, 13, DIRECTION.down);
        private Sensor stationSensor2 = new Sensor(14, 11, DIRECTION.down);
        private Sensor stationSensor3 = new Sensor(14, 5, DIRECTION.up);
        private Sensor stationSensor4 = new Sensor(14, 3, DIRECTION.up);

        public Train(int trainId, int initialSpeed, TSimInterface tsi, HashSet<Switch> switches) {
            this.trainId = trainId;
            this.initialSpeed = initialSpeed;
            this.tsi = tsi;
            this.switches = switches;
        }

        public DIRECTION getTrainDirection() {
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

        private void stopAtStation() throws Exception {
            tsi.setSpeed(trainId, 0);
            TimeUnit.MILLISECONDS.sleep(1000 + 2 * simSpeed * Math.abs(initialSpeed));
            initialSpeed *= -1;
            tsi.setSpeed(trainId, initialSpeed);
        }

        @Override
        public void run() {
            try {
                tsi.setSpeed(trainId, initialSpeed);

                while (true) {

                    SensorEvent e = tsi.getSensor(trainId);

                    if (e.getStatus() == SensorEvent.INACTIVE)
                        continue;

                    //Special cases to check if the sensors triggered are the one in the station
                    if (stationSensor1.isOwnerOfEvents(e) || stationSensor2.isOwnerOfEvents(e)) {
                        System.err.println("inside if 1");
                        if (getTrainDirection() == stationSensor1.relativeToSwitch) {
                            stopAtStation();
                            continue;
                        }
                    } else if (stationSensor3.isOwnerOfEvents(e) || stationSensor4.isOwnerOfEvents(e)) {
                        System.err.println("inside if 2");
                        if (getTrainDirection() == stationSensor3.relativeToSwitch) {
                            stopAtStation();
                            continue;
                        }
                    }

                    for (Switch s : switches) {
                        for (Sensor sen : s.sensors) {
                            if (sen.isOwnerOfEvents(e)) {
                                DIRECTION trainDirection = getTrainDirection();

                                if (!s.isSpecialCase()) {
                                    s.acquireCS(trainDirection, trainId, initialSpeed);
                                    s.releaseCS(trainDirection, trainId, sen);
                                } else {
                                    s.handleSpecialCase(trainId, initialSpeed);
                                }
                            }
                        }
                    }
                }
            } catch (Exception e) {
                e.printStackTrace();
            }
        }
    }

    class Switch {

        TSimInterface tsi;
        Map<CS, DIRECTION> cssWithDirection;
        HashSet<Sensor> sensors;
        int x;
        int y;

        public Switch(TSimInterface tsi, int x, int y) {
            this.tsi = tsi;
            this.cssWithDirection = new HashMap<CS, DIRECTION>();
            this.sensors = new HashSet<Sensor>();
            this.x = x;
            this.y = y;
        }

        private void changeSwitchDirection(int direction) throws Exception {
            int realDirection = direction;
            if (this.x == 3 || this.x == 4)
                if (direction == 1)
                    realDirection = 2;
                else
                    realDirection = 1;
            tsi.setSwitch(this.x, this.y, realDirection);
        }


        public void acquireCS(DIRECTION trainDirection, int trainId, int initialSpeed) throws Exception {
            LinkedList<CS> temp = new LinkedList<CS>();
            CS currentlyIn = null;
            for (CS cs : cssWithDirection.keySet()) {
                DIRECTION csDirection = cssWithDirection.get(cs); //Getting the position of the CS compared to the switch considered

                if (cs.occupiedByTrainId == trainId)
                    currentlyIn = cs;

                if (cs.occupiedByTrainId == trainId && trainDirection == csDirection)
                    return;

                if (trainDirection == csDirection) {
                    temp.add(cs);
                }
            }

            if (temp.size() == 0)
                return;

            CS cs;
            tsi.setSpeed(trainId, 0);
            if (temp.size() == 1) {
                cs = temp.get(0);
                cs.acquire(trainId);
            } else {
                if (temp.get(0).tryAcquire(0, TimeUnit.SECONDS)) {
                    System.err.println("Succeded with try-acquire "+ temp.get(0).label);
                    cs = temp.get(0);
                    cs.occupiedByTrainId = trainId;
                } else {
                    cs = temp.get(1);
                    System.err.println("Not succeded with t.a. "+ cs.label);
                    cs.acquire(trainId);
                }
            }

            tsi.setSpeed(trainId, initialSpeed);
            if (currentlyIn != null && (currentlyIn.label.equals("one") || cs.label.equals("one") || currentlyIn.label.equals("four")
                    || cs.label.equals("four") || currentlyIn.label.equals("seven") || cs.label.equals("seven")))
                changeSwitchDirection(TSimInterface.SWITCH_LEFT);
            else
                changeSwitchDirection(TSimInterface.SWITCH_RIGHT);
            tsi.setSpeed(trainId, initialSpeed);
        }

        public void releaseCS(DIRECTION trainDirection, int trainId, Sensor sen) throws Exception {
            for (CS cs : cssWithDirection.keySet()) {

                DIRECTION csDirection = cssWithDirection.get(cs); //Getting the position of the CS compared to the switch considered
                if (trainDirection != csDirection) {
                    //Conditional statement that checks if the CS that we left (before the switch) was occupied by ourself
                    //so it's released after exiting (after the switch)
                    if (cs.occupiedByTrainId == trainId && sen.relativeToSwitch == trainDirection) {
                        System.err.println("Releasing lock");
                        cs.release();
                    }
                }
            }
        }

        public void addCS(CS cs, DIRECTION dir) {
            cssWithDirection.put(cs, dir);
        }

        public void addSensor(Sensor s) {
            sensors.add(s);
        }

        public boolean isSpecialCase() {
            return (cssWithDirection.get(cssWithDirection.keySet().iterator().next()) == DIRECTION.noDirection);
        }

        public void handleSpecialCase(int trainId, int initialSpeed) throws Exception {
            CS cs = cssWithDirection.keySet().iterator().next();
            if (cs.occupiedByTrainId == trainId)
                cs.release();
            else {
                tsi.setSpeed(trainId, 0);
                cs.acquire(trainId);
                tsi.setSpeed(trainId, initialSpeed);
            }
        }
    }

    class Sensor {
        int x;
        int y;
        DIRECTION relativeToSwitch;

        public Sensor(int x, int y, DIRECTION direction) {
            this.x = x;
            this.y = y;
            this.relativeToSwitch = direction;
        }

        public boolean isOwnerOfEvents(SensorEvent s) {
            return this.x == s.getXpos() && this.y == s.getYpos();
        }
    }

    class CS extends Semaphore {
        String label;
        int occupiedByTrainId;

        public CS(int permits, boolean fair, String label) {
            super(permits, fair);
            this.label = label;
        }

        public void acquire(int trainId) throws InterruptedException {
            super.acquire();
            System.err.println("CS " + label + " acquired by " + trainId);
            this.occupiedByTrainId = trainId;
        }

        @Override
        public void release() {
            System.err.println("CS " + label + " released by " + occupiedByTrainId);
            super.release();
            occupiedByTrainId = -1;
        }
    }
}