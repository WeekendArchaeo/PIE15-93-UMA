/*
  MotPotLedPIE
  Turns on the even LEDS of the UMA Shield at different frequencies,
  depending on the value read from the UMA Shield potentiometer (trimmer).
  Then moves the servo to different positions according to the trimmer
  value read.

  This example code is in the public domain.

  Based on the Blink and the Sweep examples included in the Arduino IDE
  
  Ana Cruz-Martin
  March 2017
*/

#include <Servo.h>

Servo myservo;  // create servo object to control a servo
int pos = 0;    // variable to store the servo position

// mapping the Arduino pins to the UMA Shield trimmer
int trimmer = A5;
int trimmerRead = 0;

// mapping the Arduino digital pins to the UMA Shield LEDs
int LEDe1 = 10;
int LEDe2 = 8;
int LEDe3 = 5;
int LEDe4 = A4;


void setup() {
  myservo.attach(9);  // attaches the servo on pin 9 to the servo object
  Serial.begin(9600);
  // initialize digital pins as outputs
  pinMode(LEDe1, OUTPUT);
  pinMode(LEDe2, OUTPUT);
  pinMode(LEDe3, OUTPUT);
  pinMode(LEDe4, OUTPUT);
}

// blinks the specified LED a certain amount of time
void blinkLED(int led, int timedelay)
{
  digitalWrite(led, HIGH);   // turn the LED on (HIGH is the voltage level)
  delay(timedelay);          // waits for the specified delay
  digitalWrite(led, LOW);   // turn the LED off by making the voltage LOW
  delay(timedelay);         // waits for the specified delay
}


void loop() {
  trimmerRead = analogRead(trimmer);
  if (trimmerRead < 256)
  {
    blinkLED(LEDe1,1000); // wait for a second
    for (pos = 0; pos <= 45; pos += 1)  // goes from 0 degrees to 45 degrees in steps of 1 degree
    { 
       myservo.write(pos);              // tell servo to go to position in variable 'pos'
       delay(15);                       // waits 15ms for the servo to reach the position
    }
  }
  else if ((trimmerRead >= 256) & (trimmerRead < 512))
  {
    blinkLED(LEDe2,750);  // wait for 750 milliseconds
    for (pos = 45; pos <= 90; pos += 1) // goes from 45 degrees to 90 degrees in steps of 1 degree
    {
      myservo.write(pos);              // tell servo to go to position in variable 'pos'
      delay(15);                       // waits 15ms for the servo to reach the position
    }
  }
  else if ((trimmerRead >= 512) & (trimmerRead < 768))
  {
    blinkLED(LEDe3,500);  // wait for 500 milliseconds
    for (pos = 90; pos <= 135; pos += 1) // goes from 90 degrees to 135 degrees in steps of 1 degree
    {
      myservo.write(pos);              // tell servo to go to position in variable 'pos'
      delay(15);                       // waits 15ms for the servo to reach the position
    }
  }
  else
  {
   blinkLED(LEDe4,250); // wait for 250 milliseconds
   for (pos = 120; pos <= 180; pos += 1) // goes from 120 degrees to 180 degrees in steps of 1 degree
   { 
      myservo.write(pos);              // tell servo to go to position in variable 'pos'
      delay(15);                       // waits 15ms for the servo to reach the position
   }
  }
  delay(100);
}
