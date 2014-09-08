import TSim.CommandException;
import TSim.SensorEvent;
import TSim.TSimInterface;

import java.util.concurrent.Executor;
import java.util.concurrent.Executors;
import java.util.concurrent.Semaphore;
import java.util.concurrent.TimeUnit;
import java.util.*;

public class Lab1 {

    int simSpeed;

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
    	
    	CS six = new CS(1, true, "six");
    	CS seven = new CS(1, true, "seven");
    	CS eight = new CS(1, true, "eight");
    	CS nine = new CS(1, true, "nine");
    	    	

    	CS csOne = new CS(1, true, "one");
    	csOne.addSensor(new Sensor(2,11));
    	CS csTwo = new CS(1, true, "two");
    	csTwo.addSensor(new Sensor(2,11));
    	
    	
    	CS csThree = new CS(1, true, "three");
    	csThree.addSensor(new Sensor(3,12));
    	csThree.addSensor(new Sensor(4,11));
    	csThree.addSensor(new Sensor(4,10));
    	csThree.addSensor(new Sensor(5,9));

    	CS csFour = new CS(1, true, "four");
    	csFour.addSensor(new Sensor(3,9));
    	csFour.addSensor(new Sensor(16,9));
  
    	CS csFive = new CS(1, true, "five");
    	csFive.addSensor(new Sensor(3,9));
    	csFive.addSensor(new Sensor(16,9));
  
    	  
    	CS csSix = new CS(1, true, "six");
    	csSix.addSensor(new Sensor(15,10));
    	csSix.addSensor(new Sensor(14,9));
    	csSix.addSensor(new Sensor(17,8));
    	csSix.addSensor(new Sensor(16,7));
    	
    	CS csSeven = new CS(1, true, "seven");
    	csSeven.addSensor(new Sensor(18,7));

    	CS csEight = new CS(1, true, "eight");
    	csEight.addSensor(new Sensor(18,7));
    	
    	CS csNine = new CS(1, true, "nine");
    	csNine.addSensor(new Sensor(8,8));
    	csNine.addSensor(new Sensor(7,7));
    	csNine.addSensor(new Sensor(9,7));
    	csNine.addSensor(new Sensor(8,6));
    	
    	Switch one = new Switch(tsi);
    	one.addCS(csOne);
    	one.addCS(csTwo);
    	one.addCS(csThree);
    	
    	Switch two = new Switch(tsi);
    	two.addCS(csThree);
    	two.addCS(csFour);
    	two.addCS(csFive);
    	
    	Switch three = new Switch(tsi);
    	three.addCS(csFour);
    	three.addCS(csFive);
    	three.addCS(csSix);
    	
    	Switch four = new Switch(tsi);
    	four.addCS(csSix);
    	four.addCS(csSeven);
    	four.addCS(csEight);

    	Switch five = new Switch(tsi);
    	five.addCS(csNine);
    	
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
                    for(Switch s : switches) {
                    	for(CS cs : s.getCS()) {
                    		for(Sensor sensor : cs.getSenors()) {
                    			if (sensor.equals(e)) {
                    				System.err.println(e);
                    			}
                    		}
                    	}
                    }
                    
                    //System.err.println(e);
                    //initialSpeed *= -1;
                    
                    //tsi.setSpeed(trainId, 0);

                    //TimeUnit.MILLISECONDS.sleep(3 * simSpeed * Math.abs(initialSpeed));

                    //tsi.setSpeed(trainId, initialSpeed);

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
        
        @Override
        public boolean equals(Object obj) {
        	SensorEvent s = (SensorEvent) obj;
        	return this.x == s.getXpos() && this.y == s.getYpos(); 
        }
    }

    class CS extends Semaphore{
    	
    	String label;
    	HashSet<Sensor> sensors;

        public CS(int permits, String label) {
            super(permits);
            this.label = label;
            this.sensors = new HashSet<Sensor>();
        }

        public CS(int permits, boolean fair, String label) {
            super(permits, fair);
            this.label = label;
            this.sensors = new HashSet<Sensor>();
        }
        
        public void addSensor(Sensor sensor) {
        	sensors.add(sensor);
        }
        
        public HashSet<Sensor> getSenors() {
        	return this.sensors;
        }
    }
    
    class Switch {
    	
    	TSimInterface tsi;
    	Collection<CS> css;
    	
    	public Switch(TSimInterface tsi) {
    		this.tsi = tsi;
    		this.css = new HashSet<CS>();
    	}
    	
    	public void addCS(CS cs) {
    		css.add(cs);
    	}
    	
    	public Collection<CS> getCS() {
    		return this.css;
    	}
    }

}


