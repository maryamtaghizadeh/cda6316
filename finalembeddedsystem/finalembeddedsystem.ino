#include <Wire.h>
#include <LiquidCrystal_I2C.h>
#include <Wire.h>
#include "MAX30105.h"
#include "heartRate.h"
#include <Keypad.h>


/***************** LCD ********************/
// Set the LCD address to 0x27 for a 16 chars and 2 line display
LiquidCrystal_I2C lcd(0x27, 16, 2);
char child_pulse_range[16] = " /75-115 BPM";
char adult_pulse_range[16] = " /60-100 BPM";
int child_low = 75;
int child_high = 115;
int adult_low = 60;
int adult_high = 100;
/***************** LCD ********************/


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


/***************** KEYPAD ********************/
const byte ROWS = 4; /* four rows */
const byte COLS = 4; /* four columns */
/* define the symbols on the buttons of the keypads */
char hexaKeys[ROWS][COLS] = {
  {'1','2','3','A'},
  {'4','5','6','B'},
  {'7','8','9','C'},
  {'*','0','#','D'}
};
/* initialize an instance of class NewKeypad */
byte rowPins[ROWS] = {19, 18, 5, 17}; // GPIO18, GPIO5, GPIO17, GPIO16 connect to the row pins
byte colPins[COLS] = {16, 4, 0, 2};
Keypad customKeypad = Keypad(makeKeymap(hexaKeys), rowPins, colPins, ROWS, COLS); 
char age[3] = "aa";
// char exercise[2] = "a";
/***************** KEYPAD ********************/


/***************** LOGIC ********************/
int AGE_SETUP = 1;
int PULSE_CHECK = 3;
int STATE = AGE_SETUP;
String bpm_state_message = "";
String bpm_normal = "bpm normal";
String bpm_low    = "bpm low   ";
String bpm_high   = "bpm high  ";
/***************** LOGIC ********************/


/***************** SETUP ********************/
/***************** SETUP ********************/
void setup()
{
  Serial.begin(115200);

  /******* LCD **********/
  lcd.begin();
  // Turn on the blacklight
  lcd.setBacklight((uint8_t)1);
  /******* LCD **********/


  /********* HEART RATE *********/
  // Initialize sensor
  if (!particleSensor.begin(Wire, I2C_SPEED_FAST)) //Use default I2C port, 400kHz speed
  {
    Serial.println("MAX30105 was not found. Please check wiring/power. ");
    while (1);
  }
  Serial.println("Place your index finger on the sensor with steady pressure.");

  particleSensor.setup(); //Configure sensor with default settings
  particleSensor.setPulseAmplitudeRed(0x0A); //Turn Red LED to low to indicate sensor is running
  particleSensor.setPulseAmplitudeGreen(0); //Turn off Green LED
  /********* HEART RATE *********/
}

void reset_age(char* arr, char* default_value)
{
  strcpy(arr, default_value);
} 

void loop(){

  if (STATE == AGE_SETUP){
    // Prompt for age on LCD
    lcd.setCursor(0,0);
    lcd.print("Enter your age:");
    lcd.setCursor(0,1);
    lcd.print("Embedded Systems");

    char current_key = customKeypad.getKey();

    if (current_key){
      if (age[0] != 'a' and age[1] != 'a'){
        Serial.println((String) "Age is set to: " + age);
        STATE = PULSE_CHECK;
        // reset_age(age, "aa");
      } else if (age[0] != 'a'){
        age[1] = current_key;
      } else {
        age[0] = current_key;
      }
    }
  }

  if (STATE == PULSE_CHECK){
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

    if ((int)age <= 9){
      if (beatsPerMinute < child_low){
        bpm_state_message = bpm_low;
      } else if (child_low <= beatsPerMinute < child_high) {
        bpm_state_message = bpm_normal;
      } else {
        bpm_state_message = bpm_high;
      }
    } else {
      if (beatsPerMinute < adult_low){
        bpm_state_message = bpm_low;
      } else if (adult_low <= beatsPerMinute < adult_high) {
        bpm_state_message = bpm_normal;
      } else {
        bpm_state_message = bpm_high;
      }
    }

    Serial.print("IR=");
    Serial.print(irValue);
    Serial.print(", BPM=");
    Serial.print(beatsPerMinute);
    Serial.print(", Avg BPM=");
    Serial.print(beatAvg);

    if (irValue > 50000){
      lcd.setCursor(0,0);
      lcd.print("BPM: " + (String)beatsPerMinute + "            ");
      lcd.setCursor(0,1);
      lcd.print(bpm_state_message + "     ");
    } else {
      Serial.print(" No finger?");

      lcd.setCursor(0,0);
      lcd.print("Put your finger ");
      lcd.setCursor(0,1);
      lcd.print("on the HR sensor     ");
    }
    Serial.println();
  }
  
}