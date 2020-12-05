#ifndef TRANSLATE_H
#define TRANSLATE_H

#include "util.h"
#include "absyn.h"
#include "temp.h"
#include "frame.h"

/* Lab5: your code below */

/**
 * Tr_exp
 */
typedef struct Tr_exp_ *Tr_exp;
typedef struct Tr_expList_ *Tr_expList;
/********************************************************** */
/**
 * Tr_access
 */
typedef struct Tr_access_ *Tr_access;
typedef struct Tr_accessList_ *Tr_accessList;
/********************************************************** */
/**
 * Tr_level
 */
typedef struct Tr_level_ *Tr_level;
/********************************************************** */
/**
 * patchList
 */
typedef struct patchList_ *patchList;
/********************************************************** */
/**
 * conditional expressions
 */
struct Cx 
{
	patchList trues; 
	patchList falses; 
	T_stm stm;
};
/********************************************************** */
/**
 * Tr_exp_ struct
 */
struct Tr_exp_ {
	enum {Tr_ex, Tr_nx, Tr_cx} kind;
	union {T_exp ex; T_stm nx; struct Cx cx; } u;
};
struct Tr_expList_ {
    Tr_exp head;
    Tr_expList tail;
};
/********************************************************** */
/**
 * Tr_access_ struct
 */
struct Tr_access_ {
	Tr_level level;
	F_access access;
};
struct Tr_accessList_ {
	Tr_access head;
	Tr_accessList tail;	
};
/********************************************************** */
/**
 * Tr_level_ struct
 */
struct Tr_level_ {
	F_frame frame;
	Tr_level parent;
    Tr_accessList fmls;
	Temp_label name;
};
/********************************************************** */
/**
 * patchList_ struct
 */
struct patchList_ 
{
	Temp_label *head; 
	patchList tail;
};
/********************************************************** */
/**
 * Tr_exp interface
 */
Tr_expList Tr_ExpList(Tr_exp head, Tr_expList tail);
/********************************************************** */
/**
 * Tr_access interface
 */
Tr_access Tr_Access(Tr_level level, F_access access);
Tr_accessList Tr_AccessList(Tr_access head, Tr_accessList tail);
Tr_access Tr_allocLocal(Tr_level level, bool escape);
Tr_accessList Tr_formals(Tr_level level);
/********************************************************** */
/**
 * Tr_level interface
 */
Tr_level Tr_newLevel(Tr_level parent, Temp_label name, U_boolList formals);
Tr_level Tr_outermost(void);
/********************************************************** */
/**
 * patchList interface
 */
static patchList PatchList(Temp_label *head, patchList tail);
void doPatch(patchList tList, Temp_label label);
patchList joinPatch(patchList first, patchList second);
/********************************************************** */
/**
 * expressions interface
 */
static Tr_exp Tr_Ex(T_exp ex);
static Tr_exp Tr_Nx(T_stm nx);
static Tr_exp Tr_Cx(patchList trues, patchList falses, T_stm stm);

static T_exp unEx(Tr_exp e);
static T_stm unNx(Tr_exp e);
static struct Cx unCx(Tr_exp e);
/********************************************************** */
/**
 * IR interface
 */
Tr_exp Tr_simpleVar(Tr_access a, Tr_level l);
Tr_exp Tr_fieldVar(Tr_exp b, int off);
Tr_exp Tr_subscriptVar(Tr_exp b, Tr_exp i);

Tr_exp Tr_nilExp();
Tr_exp Tr_intExp(int i);
Tr_exp Tr_stringExp(string s);
Tr_exp Tr_callExp(Temp_label label, Tr_level callee, Tr_level caller, Tr_expList args);
Tr_exp Tr_binExp(A_oper op, Tr_exp left, Tr_exp right);
Tr_exp Tr_relExp(A_oper op, Tr_exp left, Tr_exp right);
Tr_exp Tr_recordExp(int n, Tr_expList fields);
Tr_exp Tr_seqExp(Tr_expList exps);
Tr_exp Tr_assignExp(Tr_exp lval, Tr_exp v);
Tr_exp Tr_ifExp(Tr_exp test, Tr_exp then, Tr_exp elsee);
Tr_exp Tr_whileExp(Tr_exp test, Tr_exp body, Temp_label done);
Tr_exp Tr_forExp(Tr_access i, Tr_exp lo, Tr_exp hi, Tr_exp body, Temp_label done);
Tr_exp Tr_breakExp(Temp_label done);
Tr_exp Tr_arrayExp(Tr_exp size, Tr_exp init);
Tr_exp Tr_noExp();
/********************************************************** */
/**
 * fragment
 */
void Tr_procEntryExit(Tr_level level, Tr_exp body, Tr_accessList formals);
F_fragList Tr_getResult(void);
/********************************************************** */
/**
 * utils
 */
void Tr_print(Tr_exp e);

#endif
