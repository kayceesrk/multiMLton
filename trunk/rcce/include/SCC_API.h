// 
// Copyright 2010 Intel Corporation
// 
//    Licensed under the Apache License, Version 2.0 (the "License");
//    you may not use this file except in compliance with the License.
//    You may obtain a copy of the License at
// 
//        http://www.apache.org/licenses/LICENSE-2.0
// 
//    Unless required by applicable law or agreed to in writing, software
//    distributed under the License is distributed on an "AS IS" BASIS,
//    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
//    See the License for the specific language governing permissions and
//    limitations under the License.
// 
// Project       : bareMetal BIOS
// File name     : config.h
// Author        : mriepen
// Date          : 2008-06-24
// Revision      : 1.01
// 
// Description   : Header file for config.c
// 
// Revision history:
// 
// mri 1.01 2008-06-24
// - Initial implementation
// 
// ------------------------------------------------------------------------------------------------
#ifndef __CONFIG_H__
#define __CONFIG_H__

// #########################################################################################
//  ____                  _           _
// / ___| _   _ _ __ ___ | |__   ___ | |___
// \___ \| | | | '_ ` _ \| '_ \ / _ \| / __|
//  ___) | |_| | | | | | | |_) | (_) | \__ \
// |____/ \__, |_| |_| |_|_.__/ \___/|_|___/
//        |___/
// 
// #########################################################################################

// Define start address of pagetable (PAGE_DIR_BASE should be equal to or above to STACK_TOP setting 
// in "../../bootcode/bootImage/defines.h" and needs to be 1025*4KB below the max private address) 
// and the number of available private slots... E.g.:
// Private space for 2 cores on 1GB of memory (MCEMU single node): 0x00000000 to 0x29ffffffff (30 slots = 480MB)
// Private space for 8 cores on 1GB of memory (MCEMU multi  node): 0x00000000 to 0x05ffffffff ( 6 slots =  96MB)
#define PRIV_SLOTS 6
#define PAGE_DIR_BASE 0x04B00000

// Symbols for MPB malloc
#define MPBADDRBITS 13
#define MPBSIZE     (1<<MPBADDRBITS)

#ifdef SHMADD
// 64MB
//#define SHMADDRBITS 26
// 128MB
//#define SHMADDRBITS 27
// 256MB
//#define SHMADDRBITS 28
// 512MB
#define SHMADDRBITS 29

#define SHMSIZE     (1<<SHMADDRBITS)

// 960MB
//#define SHMSIZE 0x3C000000 
#else
#define SHMADDRBITS 26
#define SHMSIZE     (1<<SHMADDRBITS)
#endif

#define NUM_ROWS 4
#define NUM_COLS 6
#define NUM_CORES 2
#define CRB_ADDR(x,y) (CRB_X0_Y0+(0x01000000*x)+(0x01000000*NUM_COLS*y))
#define MPB_ADDR(x,y,z) (MPB_X0_Y0+(0x01000000*x)+(0x01000000*NUM_COLS*y)+(MPBSIZE*z))

#define TID(x,y) ((y<<4)+x)
#define X_TID(tid) (tid&0x0f)
#define Y_TID(tid) (tid>>4)

#define PID(x,y,core) ((NUM_CORES*NUM_COLS*y)+(NUM_CORES*x)+core)
#define X_PID(pid) ((pid/NUM_CORES)-(NUM_COLS*Y_PID(pid)))
#define Y_PID(pid) ((pid/NUM_CORES)/NUM_COLS)
#define Z_PID(pid) (pid%NUM_CORES)

// Symbols for shared memory
#ifdef SHMADD
#define SHM_X0_Y0 0x84000000
#define SHM_X5_Y0 0x85000000
#define SHM_X0_Y2 0x86000000
#define SHM_X5_Y2 0x87000000
#else
#define SHM_X0_Y0 0x80000000
#define SHM_X5_Y0 0x81000000
#define SHM_X0_Y2 0x82000000
#define SHM_X5_Y2 0x83000000
#endif

#define SHM_ADDR  SHM_X0_Y0

// Symbols for MPB addresses
#define MPB_X0_Y0 0xc0000000
#define MPB_X1_Y0 0xc1000000
#define MPB_X2_Y0 0xc2000000
#define MPB_X3_Y0 0xc3000000
#define MPB_X4_Y0 0xc4000000
#define MPB_X5_Y0 0xc5000000
#define MPB_X0_Y1 0xc6000000
#define MPB_X1_Y1 0xc7000000
#define MPB_X2_Y1 0xc8000000
#define MPB_X3_Y1 0xc9000000
#define MPB_X4_Y1 0xca000000
#define MPB_X5_Y1 0xcb000000
#define MPB_X0_Y2 0xcc000000
#define MPB_X1_Y2 0xcd000000
#define MPB_X2_Y2 0xce000000
#define MPB_X3_Y2 0xcf000000
#define MPB_X4_Y2 0xd0000000
#define MPB_X5_Y2 0xd1000000
#define MPB_X0_Y3 0xd2000000
#define MPB_X1_Y3 0xd3000000
#define MPB_X2_Y3 0xd4000000
#define MPB_X3_Y3 0xd5000000
#define MPB_X4_Y3 0xd6000000
#define MPB_X5_Y3 0xd7000000
#define MPB_OWN   0xd8000000

// Symbols for CRB addresses
#define CRB_X0_Y0 0xe0000000
#define CRB_X1_Y0 0xe1000000
#define CRB_X2_Y0 0xe2000000
#define CRB_X3_Y0 0xe3000000
#define CRB_X4_Y0 0xe4000000
#define CRB_X5_Y0 0xe5000000
#define CRB_X0_Y1 0xe6000000
#define CRB_X1_Y1 0xe7000000
#define CRB_X2_Y1 0xe8000000
#define CRB_X3_Y1 0xe9000000
#define CRB_X4_Y1 0xea000000
#define CRB_X5_Y1 0xeb000000
#define CRB_X0_Y2 0xec000000
#define CRB_X1_Y2 0xed000000
#define CRB_X2_Y2 0xee000000
#define CRB_X3_Y2 0xef000000
#define CRB_X4_Y2 0xf0000000
#define CRB_X5_Y2 0xf1000000
#define CRB_X0_Y3 0xf2000000
#define CRB_X1_Y3 0xf3000000
#define CRB_X2_Y3 0xf4000000
#define CRB_X3_Y3 0xf5000000
#define CRB_X4_Y3 0xf6000000
#define CRB_X5_Y3 0xf7000000
#define CRB_OWN   0xf8000000

// Symbol for RPC
#define RPC_BASE  0xfb000000

// Symbols for CRB sub-addresses
#define GLCFG0   0x010
#define GLCFG1   0x018
#define L2CFG0   0x020
#define L2CFG1   0x028
#define SENSOR   0x040
#define GCBCFG   0x080
#define MYTILEID 0x100
#define LOCK0    0x200
#define LOCK1    0x400
#define LUT0     0x00800
#define LUT1     0x01000

// Symbols for GLSTATn bit positions and ranges
// of status values (Read only)
#define GLSTAT_RANGE         26:13
#define GLSTAT_XBP3_BIT      26
#define GLSTAT_XBP2_BIT      25
#define GLSTAT_XPM1_BIT      24
#define GLSTAT_XPM0_BIT      23
#define GLSTAT_XIERRNN_BIT   22
#define GLSTAT_XFERRNN_BIT   21
#define GLSTAT_XPRDY_BIT     20
#define GLSTAT_XSMIACTNN_BIT 19
#define GLSTAT_SHDWN_BIT     18
#define GLSTAT_FLUACK_BIT    17
#define GLSTAT_HALT_BIT      16
#define GLSTAT_WRBACK_BIT    15
#define GLSTAT_FLUSH_BIT     14
#define GLSTAT_BRTRMSG_BIT   13

// Symbols for GLCFGn bit positions and ranges
// of configuration and IRQ values (Read-Write)
#define GLCFG_RANGE         12:00
#define GLCFG_XPICD_RANGE   12:11
#define GLCFG_CPUTYP_BIT    10
#define GLCFG_XA20MNN_BIT   09
#define GLCFG_XSMINN _BIT   08
#define GLCFG_XSTPCLKNN_BIT 06
#define GLCFG_XRSNN_BIT     05
#define GLCFG_XIGNNENN_BIT  04
#define GLCFG_XFLSHNN_BIT   03
#define GLCFG_XINIT_BIT     02
#define GLCFG_XINTR_BIT     01
#define GLCFG_XNMI_BIT      00

// Sybols for L2CFGn bit positions and ranges
#define L2CFG_RANGE              13:00
#define L2CFG_STOPL2CCCLK_BIT    13
#define L2CFG_STOPL2ARRAYCLK_BIT 12
#define L2CFG_BLFLOATEN_BIT      11
#define L2CFG_WLSLPEN_BIT        10
#define L2CFG_WTSLPEN_BIT        09
#define L2CFG_FLIPEN_BIT         08
#define L2CFG_DATAECCEN_BIT      07
#define L2CFG_TAGECCEN_BIT       06
#define L2CFG_SLPBYPASS_BIT      05
#define L2CFG_WAYDISABLE_BIT     04
#define L2CFG_BBL2SLPPGM_RANGE   03:00

// Sybols for SENSOR bit positions and ranges
#define SENSOR_EN_BIT                13
#define SENSOR_GATE_PULSE_CNT_RANGE  12:00

// Sybols for GCBCFG bit positions and ranges
#define GCBCFG_RXB_CLKRATIO_RANGE    25:19
#define GCBCFG_TILE_CLKRATIO_RANGE   18:12
#define GCBCFG_TILE_CLKDIV_RANGE     11:08
#define GCBCFG_L2_1_SYNCRESETEN_BIT  07
#define GCBCFG_L2_0_SYNCRESETEN_BIT  06
#define GCBCFG_CORE1_SYNCRESETEN_BIT 05
#define GCBCFG_CORE0_SYNCRESETEN_BIT 04
#define GCBCFG_L2_1_RESET_BIT        03
#define GCBCFG_L2_0_RESET_BIT        02
#define GCBCFG_CORE1_RESET_BIT       01
#define GCBCFG_CORE0_RESET_BIT       00

// Tile-ID (Read Only bits)
#define TID_XPOS_RANGE    10:07
#define TID_YPOS_RANGE    06:03
#define TID_SYS2MIFDESTID 02:00

// Lock registers
#define LOCK_BIT 00

// MPB pointer type
//typedef volatile unsigned char* t_vcharp;

// #########################################################################################
//  _____                 _   _
// |  ___|   _ _ __   ___| |_(_) ___  _ __  ___
// | |_ | | | | '_ \ / __| __| |/ _ \| '_ \/ __|
// |  _|| |_| | | | | (__| |_| | (_) | | | \__ \
// |_|   \__,_|_| |_|\___|\__|_|\___/|_| |_|___/
// 
// #########################################################################################

// InitAPI opens the RCKMEM device drivers. This routine needs to be invoked
// once before using any other API functions! The successmessage can be disabled.
// 
// Parameter: printMessages (0: No messages / 1: Messages enabled)
// Return value: %
// 
void InitAPI(int printMessages);

// SetConfigBit writes a bit to a specified config register using read-modify-write. Only use
// function to access memory locations that are not (!) performance critical (e.g. Tile-ID).
// Use MallocConfigReg() function for performance critical memory locations!
// 
// Parameter: ConfigAddr                - Address of configuration register...
//            BitPos                    - Bit position within config register to set/reset
//            BitValue                  - Value to write to specified bit...
// 
void SetConfigBit(unsigned int ConfigAddr, int BitPos, int BitValue);

// SetConfigReg writes a value to a specified config register using a single write. Only use
// function to access memory locations that are not (!) performance critical (e.g. Tile-ID).
// Use MallocConfigReg() function for performance critical memory locations!
// 
// Parameter: ConfigAddr                - Address of configuration register...
//            RegValue                  - Value to write to specified register...
// 
void SetConfigReg(unsigned int ConfigAddr, int RegValue);

// ReadConfigReg reads a value from a specified config register using a single read. Only use
// function to access memory locations that are not (!) performance critical (e.g. Tile-ID).
// Use MallocConfigReg() function for performance critical memory locations!
// 
// Parameter: ConfigAddr                - Address of configuration register...
//            BitPos                    - Bit position within config register to set/reset
//            BitValue                  - Value to write to specified bit...
// 
int ReadConfigReg(unsigned int ConfigAddr);

// MallocConfigReg performs a memory map operation on ConfigAddr (physical address) and
// returns a virtual address that can be used in the application. Use this function to
// allocate memory locations that you access frequently!
// 
// Parameter: ConfigAddr                - Physical address of configuration register.
// 
// Return value: ConfigRegVirtualAddr   - Virtual address of configuration register.
// 
int* MallocConfigReg(unsigned int ConfigAddr);

// FreeConfigReg unmaps a memory location that has been mapped with the MallocConfigReg()
// function...
// 
// Parameter: ConfigRegVirtualAddr      - Virtual address of configuration register.
// 
void FreeConfigReg(int* ConfigRegVirtualAddr);

// MPBalloc allocates MPBSIZE bytes of MessagePassing buffer Memory at MPB_ADDR(x,y,core).
// 
// Parameter: MPB                   - Pointer to MPB area (return value, virtal address)
//            x,y,core              - Position of tile (x,y) and core...
// 
//#ifdef SCC
void MPBalloc(t_vcharp *MPB, int x, int y, int core, int isOwnMPB);
void SHMalloc(t_vcharp *SHM);
//#endif
// MPBunalloc unallocates an allocated MPB area.
// 
// Parameter: MPB             - Pointer to MPB area (virtual address)
// 
void MPBunalloc(t_vcharp *MPB);
void SHMunalloc(t_vcharp *SHM);

#ifdef SHMADD_CACHEABLE
int DCMflush();
#endif

#endif
