all: firmware.bin

firmware.ihx: firmware.c
	sdcc --std-sdcc99 -mmcs51 firmware.c

firmware.bin: firmware.ihx
	objcopy -Iihex --output-target binary $< $@

clean:
	rm -f firmware.bin firmware.ihx
