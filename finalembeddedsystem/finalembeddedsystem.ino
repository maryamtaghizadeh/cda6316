#include <Wire.h>
#include <LiquidCrystal_I2C.h>
#include <Wire.h>
#include "MAX30105.h"
#include "heartRate.h"

// Set the LCD address to 0x27 for a 16 chars and 2 line display
LiquidCrystal_I2C lcd(0x27, 16, 2);


/***************** HEART RATE ********************/
MAX30105 particleSensor;

const byte RATE_SIZE = 4; //Increase this for more averaging. 4 is good.
byte rates[RATE_SIZE]; //Array of heart rates
byte rateSpot = 0;
long lastBeat = 0; //Time at which the last beat occurred
float beatsPerMinute;
int beatAvg;
int hrToLCD = 0;
/***************** HEART RATE ********************/

void setup()
{
  Serial.begin(115200);

  /***************** LCD ********************/
  // enable_LCD();
  // Turn on the blacklight
  // lcd.setBacklight((uint8_t)1);
  /***************** LCD ********************/


  /***************** HEART RATE ********************/
  enable_HR();

  particleSensor.setup(); //Configure sensor with default settings
  particleSensor.setPulseAmplitudeRed(0x0A); //Turn Red LED to low to indicate sensor is running
  particleSensor.setPulseAmplitudeGreen(0); //Turn off Green LED
  /***************** HEART RATE ********************/
}

void enable_HR(){
  // Initialize sensor
  if (!particleSensor.begin(Wire, I2C_SPEED_FAST)) //Use default I2C port, 400kHz speed
  {
    Serial.println("MAX30105 was not found. Please check wiring/power. ");
    while (1);
  }
  Serial.println("Place your index finger on the sensor with steady pressure.");
}

void disable_HR(){
  particleSensor.shutDown();
}

void enable_LCD(){
  // initialize the LCD
  lcd.begin();
}

void disable_LCD(){
  // lcd.stop();
}

#include <Keypad.h>

const byte ROWS = 4; /* four rows */
const byte COLS = 4; /* four columns */
/* define the symbols on the buttons of the keypads */
char hexaKeys[ROWS][COLS] = {
  {'1','2','3','A'},
  {'4','5','6','B'},
  {'7','8','9','C'},
  {'*','0','#','D'}
};

char age[3] = "aa";
char child_pulse_range[16] = " /75-115 BPM";
char adult_pulse_range[16] = " /60-100 BPM";

// byte rowPins[ROWS] = {13, 12, 14, 27}; /* connect to the row pinouts of the keypad */
// byte colPins[COLS] = {26, 25, 33, 32}; /* connect to the column pinouts of the keypad */
byte rowPins[ROWS] = {19, 18, 5, 17}; // GPIO18, GPIO5, GPIO17, GPIO16 connect to the row pins
byte colPins[COLS] = {16, 4, 0, 2};


/* initialize an instance of class NewKeypad */
Keypad customKeypad = Keypad( makeKeymap(hexaKeys), rowPins, colPins, ROWS, COLS); 

int AGE_SETUP = 1;
int CHECK_PULSE = 0;

void reset_age(char* arr)
{
  strcpy(arr, "aa");
} 

void loop(){

  if (AGE_SETUP == 1){
  // First row
  // TODO: Create more meaningful prompt
  // lcd.print("Enter your age:");
  Serial.println("Enter your age:");

  // Second row
  // lcd.setCursor(0,1);
  // lcd.print("Embedded Systems");
  char current_key = customKeypad.getKey();

  if (current_key){
    if (age[0] != 'a' and age[1] != 'a'){
      Serial.println((String) "Age is set to: " + age);
      AGE_SETUP = 0;
      CHECK_PULSE = 1;
      // reset_age(age);
    } else if (age[0] != 'a'){
      age[1] = current_key;
    } else {
      age[0] = current_key;
    }
  }
  } else {
    // lcd.setCursor(0,0);
    // lcd.print((String) "Age: " + age + "         ");
    // lcd.setCursor(0,1);
    // lcd.print("Use HR Sensor.   ");
    Serial.print((String) "Age: " + age);
  }

  if (CHECK_PULSE == 1){
    long irValue = particleSensor.getIR();

    if (checkForBeat(irValue) == true)
    {
      //We sensed a beat!
      long delta = millis() - lastBeat;
      lastBeat = millis();

      beatsPerMinute = 60 / (delta / 1000.0);

      if (beatsPerMinute < 255 && beatsPerMinute > 20)
      {
        rates[rateSpot++] = (byte)beatsPerMinute; //Store this reading in the array
        rateSpot %= RATE_SIZE; //Wrap variable

        //Take average of readings
        beatAvg = 0;
        for (byte x = 0 ; x < RATE_SIZE ; x++)
          beatAvg += rates[x];
        beatAvg /= RATE_SIZE;
      }
      hrToLCD += 1;
    }
    // if (hrToLCD >= 10) {
    //   // lcd.setCursor(0,1);
    //   // lcd.print((String) beatAvg + child_pulse_range);
    //   Serial.print((String) beatAvg + child_pulse_range);
    //   hrToLCD = 0;
    // }

    Serial.print("IR=");
    Serial.print(irValue);
    Serial.print(", BPM=");
    Serial.print(beatsPerMinute);
    Serial.print(", Avg BPM=");
    Serial.print(beatAvg);

    if (irValue < 50000)
      Serial.print(" No finger?");

    Serial.println();
  }
  
}