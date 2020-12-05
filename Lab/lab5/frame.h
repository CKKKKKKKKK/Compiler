
/*Lab5: This header file is not complete. Please finish it with more definition.*/

#ifndef FRAME_H
#define FRAME_H

#include "tree.h"

extern const int F_wordSize;

/**
 * F_frame
 */
typedef struct F_frame_ *F_frame;
/********************************************************** */
/**
 * F_access
 */
typedef struct F_access_ *F_access;
typedef struct F_accessList_ *F_accessList;
/********************************************************** */
/**
 * F_frag
 */
typedef struct F_frag_ *F_frag;
typedef struct F_fragList_ *F_fragList;
/********************************************************** */
/**
 * F_access_ struct
 */
struct F_accessList_ {F_access head; F_accessList tail;};
/********************************************************** */
/**
 * F_frag_ struct
 */
struct F_frag_ {
	enum {F_stringFrag, F_procFrag} kind;
	union {
		struct {Temp_label label; string str;} stringg;
		struct {T_stm body; F_frame frame;} proc;
	} u;
};
struct F_fragList_ 
{
	F_frag head; 
	F_fragList tail;
};
/********************************************************** */
/**
 * F_frame interface
 */
F_frame F_newFrame(Temp_label name, U_boolList formals);
Temp_label F_name(F_frame f);
/********************************************************** */
/**
 * F_access interface
 */
F_access F_allocLocal(F_frame f, bool escape);
static F_access InFrame(int offset);
static F_access InReg(Temp_temp reg);
F_accessList F_AccessList(F_access head, F_accessList tail);
F_accessList F_formals(F_frame frame);
/********************************************************** */
/**
 * F_frag interface
 */
F_frag F_StringFrag(Temp_label label, string str);
F_frag F_ProcFrag(T_stm body, F_frame frame);
F_fragList F_FragList(F_frag head, F_fragList tail);
/********************************************************** */
/**
 * IR
 */
Temp_temp F_FP(void);
T_exp F_Exp(F_access acc, T_exp framePtr);
T_exp F_externalCall(string s, T_expList args);

#endif
