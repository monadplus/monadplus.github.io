---
title: (another) small form factor PC
description: Similiar to the previous blog but this time I talk about setup and tunning.
categories:
 - PC
tags:
 - Hardware
 - SFF Case
---

## Hardware

* CPU: AMD Ryzen 7 7800X3D + Thermalright AM5 Contact Frame + Thermal Grizzly Kryonaut 
* GPU: Asus GeForce RTX 4080 Super 16GB GDDR6 Noctua Edition
* Motherboard: Asus Rog Strix B650E-I
* Memory: Corsair Vengeance DDR5 2x16GB 6000MHz CL30 AMD EXPO
  * Replaced by CORSAIR Dominator Titanium DDR5 2x16GB 6000MHz CL30 AMD EXPO
* Storage: 2 x SK Hynix Gold P31 2TB M.2 2280 PCIe NVMe Gen3
* PSU: Cooler Master v850 SFF Gold
* Case: Cooler Master NR200P MAX
* Cooling: 280mm AIO with custom tubing
 * Replaced stock fans with 2 x Noctua NF-A14PWM (Top intake)
 * Noctua NF-F12 PWM + Noctua NF-A12x15 PWM chromax (Bottom exhaust)

![Portrait of the SFF PC](/assets/images/another_small_case/portrait.jpg) 

## Building process (step by step)

See [previous blog entry](/pc/2024/05/02/small-case-build/) (it's very similar).

## Setup & performance tunning

1. Boots? Yes
2. Check cpu/gpu/memories/disks are detected
3. Update BIOS to latest version
4. Install Windows 10 (Windows 11 not better)
  * Corrupted bootable drive iso image; had to burn iso again
  * Something is off, installation couldn't detect Ethernet
5. LAN/WIFI/Bluetooth doesn't work... motherboard RIP?
  * Missing motherboard drivers; go to website and install drivers; everything works :sweet:
6. Install chipset and iGPU drivers
7. Install Nvidia drivers
8. Reboot and overclock RAM i.e. enable EXPO (XMP for AMD)
  * Oooops, PC doesn't boot
  * Run MemTest86 to see if RAM is OK; RAM is OK
  * Check motherboard QVL list (had to do it before); RAM is on QVL list.. WTF
  * RAM works at 5600MHz but I want >6000MHz
  * Buy another QVL listed RAM; works flawlessly; return previous RAM[^1]
    ![both rams](/assets/images/another_small_case/ram.jpg) 
  * Run MemTest86 to validate RAM; Works fine
9. Check stock CPU & GPU
  * CPU using Cinebench R23, Prime 95, Super PI, OCCT
  * GPU using Unigine Heaven and Superposition
  * Stock works better than expected. Temps are really good for suck a small case.
  * The 7800x3d and the Noctua 4080SUPER are very efficient
10. Undervolt CPU without losing performance
  * Enable PBO2 and tune Curve Optimizer per core (-35 didn't work as always)
  * Test using:
    * Prime 95 Small FFT (both AVX enable and disabled)
    * OCCT Core Cycler
11. Undervolt GPU
  * 925mV at 2580MHz, +1250 memory clock (12750MHz), and 110% power limit
  * Disable CUDA P2 State using Nvidia Inspector
12. Benchmark again
  * Cinebench R23
  * Unigine Superposition
  * 3DMark Time Spy Extreme

## Summary

You can see that building and tuning this PC has turned out more complex than expected. But I am really happy with the results. The CPU and GPU run 10°C cooler than stock with similar performance (-+5%). Both were really efficient on stock settings but thanks to the undervolting, now I can run the fans at minimum at full throttle. This is a perfect SFF case to be put on top of your desk.




[^1]: Replacing the RAM was a pain in the ass
    ![Disassembled PC 2](/assets/images/another_small_case/disassembled_2.jpg) 
    ![Disassembled PC](/assets/images/another_small_case/disassembled.jpg) 
