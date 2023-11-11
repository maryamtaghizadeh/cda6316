#include <Wire.h>
#include <LiquidCrystal_I2C.h>

// Set the LCD address to 0x27 for a 16 chars and 2 line display
LiquidCrystal_I2C lcd(0x27, 16, 2);

void setup()
{
  // initialize the LCD
  lcd.begin();
  Serial.begin(115200);

  // Turn on the blacklight
  lcd.setBacklight((uint8_t)1);


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
char child_pulse_range[16] = "max 75-115 BPM";
char adult_pulse_range[16] = "max 60-100 BPM";

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
  lcd.print("Enter your age:");

  // Second row
  lcd.setCursor(0,1);
  lcd.print("Embedded Systems");
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
    lcd.setCursor(0,0);
    lcd.print((String) "Age: " + age + "         ");
    lcd.setCursor(0,1);
    if (String(age).toInt() <  10){
      lcd.print(child_pulse_range);
    } else {
      lcd.print(adult_pulse_range);
    }
  }
}