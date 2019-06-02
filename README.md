bSpokeLight -- custom firmware for LED spoke lights
===================================================

This project provides an alternative firmware for LED spoke lights. Currently,
only the model **YQ8003** (128 LEDs) is supported.

What is the YQ8003?
-------------------

[![See here](https://img.youtube.com/vi/oN4Q87aC5l4/0.jpg)](https://www.youtube.com/watch?v=oN4Q87aC5l4)

Why a new firmware?
-------------------

The advantage of this code over the official one is:

 * Adjustment for the magnet position, so that the displayed images are not
   rotated.
 * Adjustment for the width of the hub. With wide hubs, the original software
   produces a flickering effect with two copies of the picture overlaying each
   other.
 * More convenient loading of pictures. The official software works only on
   Windows, and is very crappy to use. Our software works on all operating
   systems, and is simple to use: Just give it a list of images and display
   durations. It even understands animated gifs.

Some features that are missing compared to the original version (but could be
added):

 * An animation that looks nice when the bike is stationary and the wheel is
   not turning.
 * Automatic shut-off.

How do I use it?
----------------

Once you installed it (see below), you run

    $ bSpokeLight --output my-firmware.bin image1.png 10 image2.png 10

to create the firmware `my-firmware.bin` which will display `image1.png` for 10
seconds and `image2.png` for 20 seconds. You can (currently) specify up to 8
images.

The images should be quadratic (otherwise they will be deformed), and ideally
use only the eight colors black, white, red, green, blue, yellow, magenta,
cyan.

To load the firmware onto your wheel, use any STC ISP flash tool, such as
[stcgal](https://github.com/grigorig/stcgal):

    ../path/to/stcgal.py my-firmware.bin

If this does not work right away, try to run `stcgal.py` directly after you
plug it in, e.g. using

    while ! test -e /dev/ttyUSB0; do true; done; ../path/to/stcgal.py my-firmware.bin

Calibrating your wheel
----------------------

Look at the wheel from the left side. Where is the magnet? Picture the hour arm
of a clock pointing that direction. Which hour is that (e.g. 10)? Pass this
number as the **rotation**.

Now rotate the wheel so that the LED strip is horizontal, and the end with the
white sensor is on the left. How far is the strip above the center of the hub?
Measure this number in “LEDs”, i.e. using another strip. Pass this number as
the **offset**. If the LED strip is actually below the hub, then the number is
negative.

Usually the middle piece of the strip is directly above or below the hub;
in that case, you can ignore this paragraph. But otherwise, you need a
**shift**. Measure how far the center of the strip is to the left of the hub.
Pass this number as the **shift**. If the strip is actually shifted to the
right, then the number is negative.

You pass these parameters when creating your firmware, for example (with a very
wide hub, and no shift):

    $ bSpokeLight --offset 7 --rotation 10.5 --output my-firmware.bin …

Installation
------------

Ready-built binaries for Linux can be found on
https://github.com/nomeata/bSpokeLight/releases

Binaries for Windows will be provided once we resolve
https://github.com/input-output-hk/haskell.nix/issues/160

Building from source
--------------------

 1. If you have not done so yet, fetch the source code and change to the
    directory containing the code:

        git clone https://github.com/nomeata/bSpokeLight
        cd bSpokeLight

 2. Install the `nix` tool, if you do not have it yet:

        bash <(curl https://nixos.org/nix/install)

 3. (Optional, but saves building time:) Install the Cachix tool, and enable
    the tttool cache (I am sharing the cache with an unrelated project of mine).

        nix-env -iA cachix -f https://cachix.org/api/v1/install
        cachix use tttool

 4. Build `bSpokeLight`:

        nix-build nix -A linux-exe

    The first time this can take a long time. Run it over night.

 5. Copy the resulting program to the current directory:

        cp result/bin/bSpokeLight .

 4. At this point, `bSpokeLight` should be ready to go. If you run

        ./bSpokeLight

    you should see a help output.

If you have any problems, you can [report an issue via GitHub](https://github.com/nomeata/bSpokeLight/issues).


Docker Container
----------------

A simpler way of building this on Linux is to use a `Docker` container:

1. To build:

        docker build -t bspoke .

2. To run:

        docker run --rm -it -v $PWD:/home bspoke bSpokeLight \
            -o fw.bin imgs/star.png 10

3. To develop:

        docker run --rm -it -v $PWD:/home bspoke bash

Hacking on bSpokeLight
----------------------

This is what I learned about the YQ8003 hardware:

 * The microcontroller is a STC12LE5A60S2 with 22MHz and 60KB of RAM. [Data
   sheet](www.stcmcu.com/datasheet/stc/stc-ad-pdf/stc12c5a60s2-english.pdf)

   It is 8051 compatible, so a lot of generic information on how to program
   this microcontroller is online.
 * The magnet triggers external interrupt 0.
 * The LEDs are controlled as follows:

   - `P3_4 = 0` enables the lights.
   - The 8 bits of `P1` indicate which groups of LEDs are addressed. Bits 0,1,2,3
     address the groups on the arm with the buttons, counted from the middle,
     while bits 7,6,5,4 address the groups on the arm with the sensor, again
     from the middle.
   - `P2`, _negated_, actually sets the LEDs of all addressed groups, with bit
     0 addressing the LEDs further out and bit 0 the one closest to the middle.
   - All LEDs always share the same color. `P3_5 = 0` is green, `P3_6 = 0` is
     red and `P3_7 = 0` is blue. At most one of these pins should be set to
     zero at a time.

Of course the actual firmware code expects the images in a particular format,
and the `bSpokeLight` program injects the images into the binary; see the code
for my choices there.

Can it support other spoke lights?
----------------------------------

Maybe, if the they are similar enough. Talk to me!

Contact
-------

Please reports bugs and missing features at the [GitHub bugtracker]. This is
also where you can find the [source code].

`bSpokeLight` was written by [Joachim Breitner] and is licensed under a
permissive MIT [license].

[GitHub bugtracker]: https://github.com/nomeata/bSpokeLight/issues
[source code]: https://github.com/nomeata/bSpokeLight
[Joachim Breitner]: http://www.joachim-breitner.de/
[license]: https://github.com/nomeata/bSpokeLight/blob/LICENSE
