#include <stc12.h>
#include <stdint.h>
#include <stdlib.h>

//__bit on = 1;

#define FRAMES 256
#define IMAGES 8
// 256 reloads per second
#define RELOAD_T1  58374
#define RELOAD_T1_HI 0xE4
#define RELOAD_T1_LO 0x06
#define DEMO10MS 10
#define DEMOSPEED 10000

#define Nop() \
     __asm     \
         nop   \
     __endasm  \

// how many timers until the next frame (row) is shown
// used initially, until the first magnet contact
// (i.e the first interrupt 0)
__code const uint16_t initial_step = 1000000/FRAMES;

__code const uint16_t timing[IMAGES] =
	{ 23*256+42,23*256+42,23*256+42,23*256+42,23*256+42,23*256+42,23*256+42,23*256+42 };

__code const uint8_t images[IMAGES][FRAMES * 3 * 8] =
	{"THIS IS WHERE THE IMAGE STARTS"};

// Are we doing the slow demo
uint8_t slow = 1;
// How many timer 1 cycles since the image started?
uint16_t counter = 0;
// How many timer 2 cycles until the next image?
uint16_t time10ms = timing[0];
// How many timer 2 cycles until the next demo step?
uint16_t demo10ms = DEMO10MS;
// Which images to show?
uint8_t image_num = 0;
// How many timer 1 cycles per frame
uint16_t step;
// How many timer 1 cycles until the next frame
uint16_t counter2;
// Which frame
uint8_t frame = 0;

// State of the demo points
int16_t demo[3] = { 20, 50, 200 };

void step_demo () {
  int16_t a = demo[0];
  int16_t b = demo[1];
  int16_t c = demo[2];

  int16_t a_ = a > 128 ? a/3 : (2 * 256 + a)/3;
  int16_t da =
    (4*abs(b - a) < abs(c - a)) ?
    ((b - a < 0) ? -2 : 2) :
    ((c - a < 0) ? -2 : 2);

  int16_t db;
  if (a > 128 && b > 128+64) db = -10;
  else if (a < 128 && b < 64) db = 10;
  else if (abs(a - b) > 128) db = (a_ - b < 0 ? -1 : 1);
  else db = (a - b < 0) ? 3 : -3;

  int16_t dc = (c % 2 == 1) ? -4 : 4;

  a += da;
  if (a < 0) a = 0;
  if (a > 255) a = 255;
  b += db;
  if (b < 0) b = 0;
  if (b > 255) b = 255;
  c += dc;
  if (c < 0) c = 0;
  if (c > 255) c = 255;

  demo[0] = a;
  demo[1] = b;
  demo[2] = c;
}

void Ext1_Isr() __interrupt(0) __using(2)
{
  //on = 1;
  if (slow) {
    if (counter < DEMOSPEED) slow = 0;
  } else {
    if (counter > 2*DEMOSPEED) slow = 1;
  }
  step = counter/((uint16_t)FRAMES - (uint16_t)(int8_t)frame);
  counter2 = step;
  counter = 0;
  frame = 0;
}

void ISR_Timer0() __interrupt(1) __using(2)
{
  counter++;
  counter2--;
  if (counter > 2*DEMOSPEED) { slow = 1; counter = DEMOSPEED; };
  if (!counter2) {
      counter2 = step;
      frame++;
  }
  TH0=0xff; // increase timer resolution
  TL0=0x80;
}

void Ext2_Isr() __interrupt(2) __using(2)
{
   IAP_CONTR = 0x60;
}

void ISR_Timer1() __interrupt(3) __using(2)
{
  if (slow) {
    demo10ms--;
    while (!demo10ms) {
      step_demo();
      demo10ms = DEMO10MS;
    }
  } else {
    time10ms--;
    while (!time10ms) {
      image_num++;
      image_num %= 8;
      time10ms = timing[image_num];
    }
  }
  TH1=RELOAD_T1_HI;
  TL1=RELOAD_T1_LO;
}



/*
P1: 8 bits for each group, positive
P2: 8 bits for each LED, negative 

P3_5 = 0: green
P3_6 = 0: red
P3_7 = 0: blue

 */

inline void red() { P3_7 = 1; P3_6 = 0; P3_5 = 1; }
inline void blue() { P3_7 = 0; P3_6 = 1; P3_5 = 1; }
inline void green() { P3_7 = 1; P3_6 = 1; P3_5 = 0; }
inline void all_leds () { P2 = 0x00; P1 = 0xff; P1 = 0x00; }
inline void clear () { P2 = 0xff; P1 = 0xff; P1 = 0x00; }

// blocks counted from the middle
// leds from the outside, already negated
inline void setlight(const uint8_t *led_ptr) {
	P2 = led_ptr[0]; P1_0 = 1; P1_0 = 0;
	P2 = led_ptr[1]; P1_1 = 1; P1_1 = 0;
	P2 = led_ptr[2]; P1_2 = 1; P1_2 = 0;
	P2 = led_ptr[3]; P1_3 = 1; P1_3 = 0;

	P2 = led_ptr[4]; P1_7 = 1; P1_7 = 0;
	P2 = led_ptr[5]; P1_6 = 1; P1_6 = 0;
	P2 = led_ptr[6]; P1_5 = 1; P1_5 = 0;
	P2 = led_ptr[7]; P1_4 = 1; P1_4 = 0;
}
#define SETLIGHT(image,p) \
	P2 = image[i++]; P1_0 = 1; P1_0 = 0; \
	P2 = image[i++]; P1_1 = 1; P1_1 = 0; \
	P2 = image[i++]; P1_2 = 1; P1_2 = 0; \
	P2 = image[i++]; P1_3 = 1; P1_3 = 0; \
                                             \
	P2 = image[i++]; P1_7 = 1; P1_7 = 0; \
	P2 = image[i++]; P1_6 = 1; P1_6 = 0; \
	P2 = image[i++]; P1_5 = 1; P1_5 = 0; \
	P2 = image[i++]; P1_4 = 1; P1_4 = 0; \

void set_demobit(int16_t a) {
  a = (a + 32) % 64;
  if (a < 32) {
    a = a + 7 - 2*(a % 8);
  }
  uint64_t bits = ~((uint64_t)1 << (uint64_t)a);
  P2 = ((char *)(&bits))[0]; P1_0 = 1; P1_0 = 0;
  P2 = ((char *)(&bits))[1]; P1_1 = 1; P1_1 = 0;
  P2 = ((char *)(&bits))[2]; P1_2 = 1; P1_2 = 0;
  P2 = ((char *)(&bits))[3]; P1_3 = 1; P1_3 = 0;
  P2 = ((char *)(&bits))[7]; P1_7 = 1; P1_7 = 0;
  P2 = ((char *)(&bits))[6]; P1_6 = 1; P1_6 = 0;
  P2 = ((char *)(&bits))[5]; P1_5 = 1; P1_5 = 0;
  P2 = ((char *)(&bits))[4]; P1_4 = 1; P1_4 = 0;
}

void draw_image() {
  while (1) {
    if (slow) {
      clear();
      P3_7 = 1; P3_6 = 0; P3_5 = 1;
      set_demobit(demo[0] / 4);
      clear();
      P3_7 = 1; P3_6 = 1; P3_5 = 0;
      set_demobit(demo[1] / 4);
      clear();
      P3_7 = 0; P3_6 = 1; P3_5 = 1;
      set_demobit(demo[2] / 4);
    } else {
      unsigned char g;
      unsigned int i;
      __code const uint8_t *current_image;

      current_image = images[image_num];
      g = frame;
      // if (g >= FRAMES) { g = FRAMES-1; }
      clear();
      P3_7 = 1; P3_6 = 0; P3_5 = 1;
      i = g*8*3;
      SETLIGHT(current_image,p)

      //current_image = images[image_num];
      //g = frame;
      // if (g >= FRAMES) { g = FRAMES-1; }
      clear();
      P3_7 = 1; P3_6 = 1; P3_5 = 0;
      i = g*8*3 + 8;
      SETLIGHT(current_image,p)

      //current_image = images[image_num];
      //g = frame;
      // if (g >= FRAMES) { g = FRAMES-1; }
      clear();
      P3_7 = 0; P3_6 = 1; P3_5 = 1;
      i = g*8*3 + 2*8;
      SETLIGHT(current_image,p)
    }
  }
}

void delay(void) {
  unsigned int j = 0;
  unsigned int g = 0;
  for(j=0;j<5;j++) {
    for(g=0;g<60000;g++) {
      Nop();
      Nop();
      Nop();
      Nop();
      Nop();
    }
  }
}

void main () {
	// Init sequence, with no timers and lots of delay, to test LEDs
	// and allow for easy flashing
	EA = 1;
	P3_4 = 0; // enable LEDS
	red(); all_leds(); delay(); clear();
	green(); all_leds(); delay(); clear();
	blue(); all_leds(); delay(); clear();
	EA = 0;

	// activate magnet?
	IT0 = 1;
	EX0 = 1;

	// reset via USB
	IT1 = 1;
	EX1 = 1;

	// Timer
	TMOD = 0x11;
	TR0 = 1;
	ET0 = 1; // enable timer interrupt
	TR1 = 1;
	ET1 = 1;

	TH1=RELOAD_T1_HI;
	TL1=RELOAD_T1_LO;

	// Unclear purpose, later the code reads from P4_2.
	//P4_0 = 1;

	step = counter2 = initial_step;

	EA = 1; // Enable interrupts

	draw_image();
}
