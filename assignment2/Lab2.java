/**
 * Lab group 5: Anders Gustafsson, Marcello Steiner
 */

import TSim.SensorEvent;
import TSim.TSimInterface;

import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.Map;
import java.util.concurrent.Executor;
import java.util.concurrent.Executors;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.locks.Condition;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantLock;

public class Lab2 {

    int simSpeed;

    // Utility field to keep track of sensors and critical sections relative position to their switch
    public enum DIRECTION {
        down, up, noDirection
    }

    public static void main(String[] args) {
        try {
            new Lab2(args);
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    public Lab2(String[] args) throws Exception {

        TSimInterface tsi = TSimInterface.getInstance();
        int train1Speed = Integer.parseInt(args[0]);
        int train2speed = Integer.parseInt(args[1]);
        simSpeed = Integer.parseInt(args[2]);

        // Initialization of all Critical sections, sensors and switches
        CS csOne = new CS("one");
        CS csTwo = new CS("two");
        //csTwo.enter(2);
        CS csThree = new CS("three");
        CS csFour = new CS("four");
        CS csFive = new CS("five");
        CS csSix = new CS("six");
        CS csSeven = new CS("seven");
        CS csEight = new CS("eight");
        // csEight.enter(1);
        CS csNine = new CS("nine");

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
        two.addSensor(new Sensor(3, 9, DIRECTION.down));
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

                    // We consider only the active events
                    if (e.getStatus() == SensorEvent.INACTIVE)
                        continue;

                    // Special cases to check if the sensors triggered are the one in the station
                    if (stationSensor1.isOwnerOfEvents(e) || stationSensor2.isOwnerOfEvents(e)) {
                        // If the train is going towards the station
                        if (getTrainDirection() == stationSensor1.relativeToSwitch) {
                            stopAtStation();
                            continue;
                        }
                    } else if (stationSensor3.isOwnerOfEvents(e) || stationSensor4.isOwnerOfEvents(e)) {
                        if (getTrainDirection() == stationSensor3.relativeToSwitch) {
                            stopAtStation();
                            continue;
                        }
                    }

                    // Loops to find the switch that contains the triggered sensor
                    for (Switch s : switches) {
                        for (Sensor sen : s.sensors) {
                            if (sen.isOwnerOfEvents(e)) {
                                DIRECTION trainDirection = getTrainDirection();
                                // If the sensor triggered are not part of the X-crossing
                                if (!s.isSpecialCase()) {
                                    s.acquireCS(trainDirection, trainId, initialSpeed);
                                    s.releaseCS(trainDirection, trainId, sen);
                                    // The sensor are related to the X-crossing
                                } else {
                                    s.handleSpecialCase(trainId, initialSpeed);
                                }
                                break;
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
        // Map of Critical section and their direction to the current switch
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
            // Direction of
            int realDirection = direction;
            // If the switch "points" north, then reverse switch flipping Left to Right
            if (this.x == 3 || this.x == 4)
                if (direction == 1)
                    realDirection = 2;
                else
                    realDirection = 1;
            tsi.setSwitch(this.x, this.y, realDirection);
        }


        public void acquireCS(DIRECTION trainDirection, int trainId, int initialSpeed) throws Exception {
            LinkedList<CS> csToAcquire = new LinkedList<CS>();
            CS currentlyIn = null;
            // Looping through the critical sections for the switch
            for (CS cs : cssWithDirection.keySet()) {
                DIRECTION csDirection = cssWithDirection.get(cs); //Getting the position of the CS compared to the switch considered

                // If the critical section is occupied by the train itself, then the train is already in the current critical section
                if (cs.occupiedByTrainId == trainId)
                    currentlyIn = cs;

                // The train is already in the current section and in the same direction, so we the train does not have to enter it again.
                if (cs.occupiedByTrainId == trainId && trainDirection == csDirection)
                    return;

                // If the train has the same direction into the switch as the critical section has, then we should enter it:
                if (trainDirection == csDirection) {
                    csToAcquire.add(cs);
                }
            }


            if (csToAcquire.size() == 0)
                return;

            CS cs;
            tsi.setSpeed(trainId, 0);
            // If there is only one upcoming critical section
            if (csToAcquire.size() == 1) {
                cs = csToAcquire.get(0);
                cs.enter(trainId);
            }
            // There are more than one critical section to choose from, we have to choose the one who is free.
            else {
                // If the first one is free, we take that one
                if (csToAcquire.get(0).tryEnter()) {
                    cs = csToAcquire.get(0);
                    cs.occupiedByTrainId = trainId;
                }
                // We take the second one
                else {
                    cs = csToAcquire.get(1);
                    cs.enter(trainId);
                }
            }

            tsi.setSpeed(trainId, initialSpeed);
            // After a train has acquired the wanted critical section, we need to check the switches
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
                        cs.leave();
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

        // If it is the X-crossing
        public boolean isSpecialCase() {
            return (cssWithDirection.get(cssWithDirection.keySet().iterator().next()) == DIRECTION.noDirection);
        }

        // Handling the X-crossing
        public void handleSpecialCase(int trainId, int initialSpeed) throws Exception {
            CS cs = cssWithDirection.keySet().iterator().next();
            if (cs.occupiedByTrainId == trainId)
                cs.enter(trainId);
            else {
                tsi.setSpeed(trainId, 0);
                cs.enter(trainId);
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

    class CS {
        String label;
        Lock lock = new ReentrantLock();
        Condition condition = lock.newCondition();
        // Train that owns the Semaphore when it is taken.
        int occupiedByTrainId;


        public CS(String label) {
            this.label = label;
        }

        public void enter(int trainId) throws InterruptedException {
            lock.lock();
            if (tryEnter()) {
                System.err.println("Inside if");
                condition.await();
            }
            this.occupiedByTrainId = trainId;

        }

        public boolean tryEnter() throws InterruptedException {
            return condition.await(1, TimeUnit.MILLISECONDS);
        }

        public void leave() {
            condition.signal();
            lock.unlock();
            // When the semaphore is free, the current train is -1.
            occupiedByTrainId = -1;
        }
    }
}