#include <stc12.h>
#include <stdint.h>

//__bit on = 1;

#define FRAMES 256
#define IMAGES 8
// 256 reloads per second
#define RELOAD_T1  58374
#define RELOAD_T1_HI 0xE4
#define RELOAD_T1_LO 0x06

// how many timers until the next frame (row) is shown
// used initially, until the first magnet contact
// (i.e the first interrupt 0)
__code const uint16_t initial_step = 1000000/FRAMES;

__code const uint16_t timing[IMAGES] =
	{ 23*256+42,23*256+42,23*256+42,23*256+42,23*256+42,23*256+42,23*256+42,23*256+42 };

__code const uint8_t images[IMAGES][FRAMES * 3 * 8] =
	{"THIS IS WHERE THE IMAGE STARTS"};

// How many timer 1 cycles since the image started?
uint16_t counter = 0;
// How many timer 2 cycles until the next image?
uint16_t time10ms = timing[0];
// Which images to show?
uint8_t image_num = 0;
// How many timer 1 cycles per frame
uint16_t step;
// How many timer 1 cycles until the next frame
uint16_t counter2;
// Which frame
uint8_t frame = 0;


void Ext1_Isr() __interrupt(0) __using(2)
{
  //on = 1;
  step = counter/FRAMES;
  counter2 = step;
  counter = 0;
  frame = 0;
}

void ISR_Timer0() __interrupt(1) __using(2)
{
  counter++;
  counter2--;
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
  time10ms--;
  while (!time10ms) {
	image_num++;
	image_num %= 8;
	time10ms = timing[image_num];
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

inline void clear () {
  P2 = 0xff;
  P1 = 0xff;
  P1 = 0x00;
}

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

void draw_image() {
	while (1) {
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

		current_image = images[image_num];
		g = frame;
		// if (g >= FRAMES) { g = FRAMES-1; }
		clear();
		P3_7 = 1; P3_6 = 1; P3_5 = 0;
		i = g*8*3 + 8;
		SETLIGHT(current_image,p)

		current_image = images[image_num];
		g = frame;
		// if (g >= FRAMES) { g = FRAMES-1; }
		clear();
		P3_7 = 0; P3_6 = 1; P3_5 = 1;
		i = g*8*3 + 2*8;
		SETLIGHT(current_image,p)
	}

}

void main () {
	unsigned long int x;
	uint8_t i = 0;

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

	// Wait a while (to flash it)
	x = 0x20000; while (x--) {};

	P3_4 = 0;
	P2 = 0xff;
	P1 = 0x01;
	draw_image();
}
