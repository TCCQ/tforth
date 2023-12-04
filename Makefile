# Keep it pretty basic

# TODO for some reason the linkerscript ENTRY variable isn't being respected, so file order matters here
third-qemu.elf: third.s qemu-test.s qemu-test.ld
	riscv64-elf-gcc -T qemu-test.ld -g qemu-test.s third.s -o third-qemu.elf -no-pie -fno-pic -ffreestanding -nostdlib

run: third-qemu.elf
	qemu-system-riscv64 -machine virt -m 128M -smp 1 -bios none -nographic -kernel third-qemu.elf

debug: third-qemu.elf
	qemu-system-riscv64 -s -S -machine virt -m 128M -smp 1 -bios none -nographic -kernel third-qemu.elf

clean:
	rm -f third-qemu.elf

