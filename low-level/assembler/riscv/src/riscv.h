#ifndef RISCV_H
#define RISCV_H

typedef enum 
{
  FM_R,
  FM_I,
  FM_S,
  FM_SB,
  FM_U,
  FM_UJ,
} Format;

// integer value = register number
// e.g. R_T0 = 5; it is x5
typedef enum
{
  R_ZERO,
  R_RA,
  R_SP,
  R_GP,
  R_TP,
  R_T0,
  R_T1,
  R_T2,
  R_S0,
  R_S1,
  R_A0,
  R_A1,
  R_A2,
  R_A3,
  R_A4,
  R_A5,
  R_A6,
  R_A7,
  R_S2,
  R_S3,
  R_S4,
  R_S5,
  R_S6,
  R_S7,
  R_S8,
  R_S9,
  R_S10,
  R_S11,
  R_T3,
  R_T4,
  R_T5,
  R_T6,
} Register;

typedef struct Instruction
{
  Format fmt;

  char opcode;
  char func7;
  char func3;

  Register rd;
  Register rs1;
  Register rs2;
  int imm;

  struct Instruction *next;
} Instruction;

#endif // RISCV_H