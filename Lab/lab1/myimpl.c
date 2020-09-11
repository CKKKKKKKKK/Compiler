#include "slp.h"
#include "util.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef struct table *Table_;
struct table {string id;int value;Table_ tail;};
struct intAndTable {int i;Table_ t;};
typedef struct intAndTable *IntAndTable_;

Table_ Table(string id,int value,struct table *tail);
IntAndTable_ IntAndTable(int i,Table_ t);
Table_ interpStm(A_stm s,Table_ t);
IntAndTable_ interpExp(A_exp e,Table_ t);
int lookup(Table_ t,string key);
int maxargsForExp(A_exp exp);
int maxargs(A_stm stm);
void interp(A_stm stm);

/**
 * The constructor of Table_
 */
Table_ Table(string id,int value,struct table *tail)
{
	Table_ t = checked_malloc(sizeof(*t));
	t->id = id;
	t->value = value;
	t->tail = tail;
	return t;
}

/**
 * The constructor of IntAndTable_
*/
IntAndTable_ IntAndTable(int i,Table_ t)
{
	IntAndTable_ result = checked_malloc(sizeof(*result));
	result->i = i;
	result->t = t;
	return result;
}

/**
 * interpStm: update input table t according to the input statement s 
 */
Table_ interpStm(A_stm s,Table_ t)
{
	//case 0: s is a compound statement
	if(s->kind == A_compoundStm)
	{
		Table_ result1 = interpStm(s->u.compound.stm1,t);
		Table_ result = interpStm(s->u.compound.stm2,result1);
		return result;
	}
	//case 1: s is an assign statement
	else if(s->kind == A_assignStm)
	{
		IntAndTable_ result1 = interpExp(s->u.assign.exp,t);
		Table_ result = Table(s->u.assign.id,result1->i,result1->t);
		lookup(result,"a");
		return result;
	}
	//case 2: s is a print statement
	else
	{
		A_expList expList = s->u.print.exps;
		if(expList->kind == A_lastExpList)
		{
			IntAndTable_ result1 = interpExp(expList->u.last,t);
			printf("%d\n",result1->i);
			Table_ result = result1->t;
			return result;
		}
		else
		{
			A_expList pointer = expList;
			IntAndTable_ result1 = interpExp(pointer->u.pair.head,t);
			printf("%d ",result1->i);
			pointer = pointer->u.pair.tail;
			while(pointer->kind != A_lastExpList)
			{
				result1 = interpExp(pointer->u.pair.head,result1->t);
				printf("%d ",result1->i);
				pointer = pointer->u.pair.tail;
			}
			//Assert: pointer->kind == A_lastExpList
			result1 = interpExp(pointer->u.last,result1->t);
			printf("%d\n",result1->i);
			Table_ result = result1->t;
			return result;
		}
	}
}

/**
* interpExp: interpret the expression e with table t and return an IntAndTable_ type
*/
IntAndTable_ interpExp(A_exp e,Table_ t)
{
	//case 0: e is an identifier expression
	if(e->kind == A_idExp)
	{
		int value = lookup(t,e->u.id);
		IntAndTable_ result = IntAndTable(value,t);
		return result;
	}
	//case 1: e is a number expression
	else if(e->kind == A_numExp)
	{
		IntAndTable_ result = IntAndTable(e->u.num,t);
		return result;
	}
	//case 2: e is an operation expression
	else if(e->kind == A_opExp)
	{
		IntAndTable_ result1 = interpExp(e->u.op.left,t);
		IntAndTable_ result2 = interpExp(e->u.op.right,result1->t);
		int value;
		if(e->u.op.oper == A_plus)
		{
			value = result1->i + result2->i;
		}
		else if(e->u.op.oper == A_minus)
		{
			value = result1->i - result2->i;
		}
		else if(e->u.op.oper == A_times)
		{
			value = result1->i * result2->i;
		}
		else
		{
			value = result1->i / result2->i;
		}
		IntAndTable_ result = IntAndTable(value,result2->t);
		return result;
	}
	//case 3: e is A_eseqExp
	else
	{
		Table_ newTable = interpStm(e->u.eseq.stm,t);
		IntAndTable_ result = interpExp(e->u.eseq.exp,newTable);
		return result;
	}
}

/**
 * lookup: return value according to the input key and Table_ t
*/
int lookup(Table_ t,string key)
{
	Table_ pointer = t;
	while(pointer != NULL)
	{
		if(!strcmp(pointer->id,key))
		{
			return pointer->value;
		}
		pointer = pointer->tail;
	}
}

/**
 * maxargsForExp: count the max number of print statement arguments in exp
*/
int maxargsForExp(A_exp exp)
{
	//case 0: exp is A_eseqExp
	if(exp->kind == A_eseqExp)
	{
		int result = maxargs(exp->u.eseq.stm) + maxargsForExp(exp->u.eseq.exp);
		return result;
	}
	//case 1: exp is A_opExp
	else if(exp->kind == A_opExp)
	{
		int result = maxargsForExp(exp->u.op.left) + maxargsForExp(exp->u.op.right);
		return result;
	}
	//case 2,3: exp is A_idExp,A_numExp
	else
	{
		return 0;
	}
}

int maxargs(A_stm stm)
{
	//TODO: put your code here.
	//case 0: stm is a print statement
	if(stm->kind == A_printStm)
	{
		A_expList expList = stm->u.print.exps;
		if(expList->kind == A_lastExpList)
		{
			A_exp exp = expList->u.last;
			return maxargsForExp(exp) > 1 ? maxargsForExp(exp) : 1;
		}
		else
		{
			int count = 1;
			int max = maxargsForExp(expList->u.pair.head);
			A_expList next = expList->u.pair.tail;
			while(next->kind != A_lastExpList)
			{
				count++;
				max = maxargsForExp(next->u.pair.head) > max ? maxargsForExp(next->u.pair.head) : max;
				next = next->u.pair.tail;
			}
			//Assert: next->kind == A_lastExpList
			count++;
			max = maxargsForExp(next->u.last) > max ? maxargsForExp(next->u.last) : max;
			int result = max > count ? max : count;
			return result;
		}
	}
	//case 1: stm is a compound statement
	else if(stm->kind == A_compoundStm)
	{
		int result1 = maxargs(stm->u.compound.stm1);
		int result2 = maxargs(stm->u.compound.stm2);
		int result = result1 > result2 ? result1 : result2;
		return result;
	}
	//case 2: stm is an assign statement
	else if(stm->kind == A_assignStm)
	{
		A_exp expression = stm->u.assign.exp;
		int result = maxargsForExp(expression);
		return result;
	}
	return 0;
}

void interp(A_stm stm)
{
	//TODO: put your code here.
	interpStm(stm,NULL);
}
