#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "util.h"
#include "symbol.h"
#include "temp.h"
#include "table.h"
#include "tree.h"
#include "frame.h"

/*Lab5: Your implementation here.*/

/*machine-dependent variables*/
const int F_wordSize = 8;
static const int F_formalRegNum = 6;
struct F_frame_ {
	T_stmList shift;
	int size;
	F_accessList fmls;
	Temp_label name;
};
struct F_access_ {
	enum {inFrame, inReg} kind;
	union {
		int offset; //inFrame
		Temp_temp reg; //inReg
	} u;
};

/********************************************************** */
/**
 * F_access
 */

/**
 * @brief F_allocLocal: allocate a local variable in F_frame f
 * 
 * @param f 
 * @param escape 
 * @return F_access 
 */
F_access F_allocLocal(F_frame f, bool escape)
{
	f->size += F_wordSize;
	if (escape) {
		return InFrame(-1 * f->size);
	} else {
		return InReg(Temp_newtemp());
	}
}

/**
 * @brief InFrame: create an escape variable
 * 
 * @param offset 
 * @return F_access 
 */
static F_access InFrame(int offset)
{
	F_access a = checked_malloc(sizeof(*a));

	a->kind = inFrame;
	a->u.offset = offset;

	return a;
}

/**
 * @brief InReg: create a not escape variable
 * 
 * @param reg 
 * @return F_access 
 */
static F_access InReg(Temp_temp reg)
{
	F_access a = checked_malloc(sizeof(*a));

	a->kind = inReg;
	a->u.reg = reg;

	return a;
}

/**
 * @brief F_AccessList: constructor of F_accessList
 * 
 * @param head 
 * @param tail 
 * @return F_accessList 
 */
F_accessList F_AccessList(F_access head, F_accessList tail)
{
	F_accessList a = checked_malloc(sizeof(*a));

	a->head = head;
	a->tail = tail;
	return a;
}

/**
 * @brief F_formals: get formals of F_frame f
 * 
 * @param f 
 * @return F_accessList 
 */
F_accessList F_formals(F_frame f)
{
	return f->fmls;
}

/**
 * @brief makeFormalAccessList: create F_accessList according to the given formals
 * 
 * @param formals 
 * @return F_accessList 
 */
static F_accessList makeFormalAccessList(U_boolList formals)
{
	U_boolList f = formals;
	F_accessList head = NULL;
	F_accessList tail = NULL;
	for (int i = 0; f; f=f->tail, i++) {
		F_access a = NULL;
		if (i < F_formalRegNum && !f->head) { // not escape
			a = InReg(Temp_newtemp());
		} else {
			a = InFrame((i + 1) * F_wordSize); // spare 1 word for return address
		}
		if (head == NULL) {
			head = tail = F_AccessList(a, NULL);
		} else {
			tail->tail = F_AccessList(a, NULL);
			tail = tail->tail;
		}
	}
	return head;
}
/********************************************************** */
/**
 * F_frame
 */

/**
 * @brief F_newFrame: F_frame constructor
 * 
 * @param name 
 * @param formals 
 * @return F_frame 
 */
F_frame F_newFrame(Temp_label name, U_boolList formals)
{
	F_frame f = checked_malloc(sizeof(*f));
	f->name = name;
	f->fmls = makeFormalAccessList(formals);
	f->size = 0;
	return f;
}

/**
 * @brief F_name: get name of F_frame f
 * 
 * @param f 
 * @return Temp_label 
 */
Temp_label F_name(F_frame f)
{
	return f->name;
}
/********************************************************** */
/**
 * F_frag
 */

/**
 * @brief F_StringFrag: create a string fragment
 * 
 * @param label 
 * @param str 
 * @return F_frag 
 */
F_frag F_StringFrag(Temp_label label, string str) {   
	 F_frag f = checked_malloc(sizeof(*f));
	f->kind = F_stringFrag;
	f->u.stringg.label = label;
	f->u.stringg.str = str;
	return f;                             
}                                                     

 /**
  * @brief F_ProcFrag: create a process fragment
  * 
  * @param body 
  * @param frame 
  * @return * F_frag 
  */
F_frag F_ProcFrag(T_stm body, F_frame frame) {        
	F_frag f = checked_malloc(sizeof(*f));
	f->kind = F_procFrag;
	f->u.proc.body = body;
	f->u.proc.frame = frame;
	return f;                           
}                                                     

/**
 * @brief F_FragList: constructor of F_fragList
 * 
 * @param head 
 * @param tail 
 * @return * F_fragList 
 */
F_fragList F_FragList(F_frag head, F_fragList tail) { 
	F_fragList fl = checked_malloc(sizeof(*fl));
	fl->head = head;
	fl->tail = tail;
	return fl;                                             
}  
/********************************************************** */
/**
 * IR
 */
static Temp_temp rbp = NULL;

/**
 * @brief F_FP: return frame pointer
 * 
 * @return Temp_temp 
 */
Temp_temp F_FP(void)
{
	if (rbp == NULL) {
		rbp = Temp_newtemp();
	}
	return rbp;
}

/**
 * @brief F_Exp: return tree expression of a local variable
 * 
 * @param acc 
 * @param framePtr 
 * @return T_exp 
 */
T_exp F_Exp(F_access acc, T_exp framePtr)
{
	if (acc->kind == inFrame) {
		return T_Mem(T_Binop(T_plus, framePtr, T_Const(acc->u.offset)));
	} else {
		return T_Temp(acc->u.reg);
	}
}

/**
 * @brief F_externalCall: generate tree expression to call external function
 * 
 * @param s 
 * @param args 
 * @return T_exp 
 */
T_exp F_externalCall(string s, T_expList args) {
    return T_Call(T_Name(Temp_namedlabel(s)), args);
}


