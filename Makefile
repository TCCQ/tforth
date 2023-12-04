# Keep it pretty basic

third-qemu.elf: third.s qemu-test.s qemu-test.ld
	riscv64-elf-gcc -T qemu-test.ld -g third.s qemu-test.s -o third-qemu.elf -no-pie -fno-pic -ffreestanding -nostdlib

run: third-qemu.elf
	qemu-system-riscv64 -machine virt -m 128M -smp 1 -bios none -nographic -kernel third-qemu.elf

debug: third-qemu.elf
	qemu-system-riscv64 -s -S -machine virt -m 128M -smp 1 -bios none -nographic -kernel third-qemu.elf

