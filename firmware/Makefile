all: firmware.bin

firmware.ihx: firmware.c
	sdcc --std-sdcc99 -mmcs51 firmware.c

firmware.bin: firmware.ihx
	sdobjcopy -Iihex --output-target binary $< $@

clean:
	rm -f firmware.bin \
	      firmware.ihx \
	      firmware.asm \
	      firmware.lk  \
	      firmware.map \
	      firmware.rel \
	      firmware.sym \
	      firmware.lst \
	      firmware.mem \
	      firmware.rst \

