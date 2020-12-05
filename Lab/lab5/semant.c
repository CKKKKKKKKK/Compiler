#include <stdio.h>
#include <assert.h>
#include <string.h>
#include "util.h"
#include "errormsg.h"
#include "symbol.h"
#include "absyn.h"
#include "types.h"
#include "env.h"
#include "semant.h"
#include "helper.h"
#include "translate.h"

/*Lab5: Your implementation of lab5.*/

struct expty 
{
	Tr_exp exp; 
	Ty_ty ty;
};

//In Lab4, the first argument exp should always be **NULL**.
struct expty expTy(Tr_exp exp, Ty_ty ty)
{
	struct expty e;

	e.exp = exp;
	e.ty = ty;

	return e;
}

Ty_ty actual_ty(Ty_ty t) 
{
	if (t == NULL) {
		return Ty_Void();
	}
	while(t && t->kind == Ty_name) {
		t = get_ty_name(t).ty;
	}
	return t;
}

struct expty transVar(S_table venv, S_table tenv, A_var v, Tr_level l, Temp_label neareast_break) 
{
	switch(v->kind) {
		case A_simpleVar: {
			E_enventry x = S_look(venv, get_simplevar_sym(v));
			if(!x || x->kind != E_varEntry) {
				EM_error(v->pos, "undefined variable %s", S_name(get_simplevar_sym(v)));
				return expTy(Tr_noExp(), Ty_Int());
			}
			return expTy(Tr_simpleVar(get_var_access(x), l), actual_ty(get_varentry_type(x)));
		}
	   	case A_fieldVar: {
			struct expty lvalue = transVar(venv, tenv, get_fieldvar_var(v), l, neareast_break);
			if(get_expty_kind(lvalue) != Ty_record) {
				EM_error(v->pos, "not a record type");
				return expTy(Tr_noExp(), Ty_Int());
			}

			// Find the field of the record
			Ty_fieldList fields = get_record_fieldlist(lvalue);
			int offset = 0;
			while(fields && fields->head->name != get_fieldvar_sym(v)) {
				fields = fields->tail;
				offset++;
			}
			if(fields == NULL) {
				EM_error(v->pos, "field %s doesn't exist", S_name(get_fieldvar_sym(v)));
				return expTy(Tr_noExp(), Ty_Int());
			}

			return expTy(Tr_fieldVar(lvalue.exp, offset), actual_ty(fields->head->ty));
		}
		case A_subscriptVar: {
			struct expty lvalue = transVar(venv, tenv, get_subvar_var(v), l, neareast_break);
			struct expty index = transExp(venv, tenv, get_subvar_exp(v), l, neareast_break);
			if(get_expty_kind(lvalue) != Ty_array) {
				EM_error(v->pos, "array type required");
				return expTy(Tr_noExp(), Ty_Int());
			}
			if(get_expty_kind(index) != Ty_int) {
				EM_error(v->pos, "index type is not int");
				return expTy(Tr_noExp(), Ty_Int());
			}
			return expTy(Tr_subscriptVar(lvalue.exp, index.exp), actual_ty(get_array(lvalue)));
		}
	}
	assert(0); /* should have returned from some clause of the switch */
}

int hasLoopVar(S_table venv, A_var v) {
	switch(v->kind) {
		case A_simpleVar: {
			E_enventry x = S_look(venv, v->u.simple);
			if (x->readonly) {
				EM_error(v->pos, "loop variable can't be assigned");
				return 1;
			}
			return 0;
		}
	   	case A_fieldVar:
			return hasLoopVar(venv, v->u.field.var);
		case A_subscriptVar:
			return hasLoopVar(venv, v->u.subscript.var);
	}
	assert(0); /* should have returned from some clause of the switch */
}

struct expty transExp(S_table venv, S_table tenv, A_exp a, Tr_level l, Temp_label nearest_break)
{
	switch(a->kind) {
		case A_varExp: {
			return transVar(venv, tenv, a->u.var, l, nearest_break);
		}
		case A_nilExp: {
			return expTy(Tr_nilExp(), Ty_Nil());
		}
		case A_intExp: {
			return expTy(Tr_intExp(a->u.intt), Ty_Int());
		}
		case A_stringExp: {
			return expTy(Tr_stringExp(a->u.stringg), Ty_String());
		}
		case A_callExp: {
			E_enventry x = S_look(venv, get_callexp_func(a));
			if(!x || x->kind != E_funEntry) {
				EM_error(a->pos, "undefined function %s", S_name(get_callexp_func(a)));
				return expTy(Tr_noExp(), Ty_Int());
			}

			// Check parameter types and number
			Ty_tyList expected = get_func_tylist(x);
			A_expList actual = get_callexp_args(a);
			Tr_expList args = NULL;
			Tr_expList tail = NULL;
			for( ; actual && expected; actual = actual->tail, expected = expected->tail) {
				struct expty exp = transExp(venv, tenv, actual->head, l, nearest_break);
				if(actual_ty(exp.ty)->kind != actual_ty(expected->head)->kind) {
					EM_error(actual->head->pos, "para type mismatch");
				}
				if(args == NULL) {
					args = tail = Tr_ExpList(exp.exp, NULL);
				} else {
					tail->tail = Tr_ExpList(exp.exp, NULL);
					tail = tail->tail;
				}
			}
			if(expected != NULL || actual != NULL) {
				EM_error(a->pos, "too many params in function %s", S_name(get_callexp_func(a)));
			}

			return expTy(Tr_callExp(get_func_label(x), get_func_level(x), l, args), actual_ty(get_func_res(x)));
		}
		case A_opExp: {
			A_oper oper = get_opexp_oper(a);
			struct expty left = transExp(venv, tenv, get_opexp_left(a), l, nearest_break);
			struct expty right = transExp(venv, tenv, get_opexp_right(a), l, nearest_break);
			if(oper == A_plusOp || oper == A_minusOp || oper == A_timesOp || oper == A_divideOp) {
				if(actual_ty(left.ty)->kind != Ty_int) {
					EM_error(get_opexp_leftpos(a), "integer required");
					return expTy(NULL, Ty_Int());
				}
				if(actual_ty(right.ty)->kind != Ty_int) {
					EM_error(get_opexp_rightpos(a), "integer required");
					return expTy(NULL, Ty_Int());
				}
				return expTy(Tr_binExp(oper, left.exp, right.exp), Ty_Int());
			} else if (oper == A_eqOp || oper == A_neqOp || oper == A_ltOp || oper == A_leOp || oper == A_gtOp || oper == A_geOp) {
				if(actual_ty(left.ty) != actual_ty(right.ty)) {
					EM_error(get_opexp_leftpos(a), "same type required");
				}
			} else {
				assert(0);
			}
			return expTy(Tr_relExp(oper, left.exp, right.exp), Ty_Int());
		}
		case A_recordExp: {
			Ty_ty type = S_look(tenv, get_recordexp_typ(a));
			type = actual_ty(type);
			if(type == NULL) {
				EM_error(a->pos, "undefined type %s", S_name(get_recordexp_typ(a)));
				return expTy(Tr_noExp(), Ty_Int());
			}
			if(type->kind !=Ty_record) {
				EM_error(a->pos, "not record type %s", S_name(get_recordexp_typ(a)));
				return expTy(Tr_noExp(), type);
			}

			// Check fields
			Ty_fieldList expected = type->u.record;
			A_efieldList actual = get_recordexp_fields(a);
			Tr_expList fields = NULL;
			Tr_expList tail = NULL;
			int count = 0;
			for( ; actual && expected; actual = actual->tail, expected = expected->tail, count++) {
				// Check name and type
				if(expected->head->name != actual->head->name) {
					EM_error(a->pos, "expected %s but get %s", S_name(expected->head->name), S_name(actual->head->name));
				}
				struct expty exp = transExp(venv, tenv, actual->head->exp, l, nearest_break);
				if(actual_ty(expected->head->ty) != actual_ty(exp.ty)) {
					EM_error(a->pos, "type not match");
				}
				if (fields == NULL) {
					fields = tail = Tr_ExpList(exp.exp, NULL);
				} else {
					tail->tail = Tr_ExpList(exp.exp, NULL);
					tail = tail->tail;
				}
			}
			if(expected != NULL || actual != NULL) {
				EM_error(a->pos, "field number of %s does not match", S_name(a->u.record.typ));
			}

			return expTy(Tr_recordExp(count, fields), type);
		}
		case A_seqExp: {
			A_expList seq = get_seqexp_seq(a);
			if (seq == NULL) {
				return expTy(Tr_noExp(), Ty_Void());
			}
			struct expty exp = expTy(NULL, Ty_Void());
			Tr_expList expList = NULL;
			Tr_expList tail = NULL;
			while(seq != NULL) {
				exp = transExp(venv, tenv, seq->head, l, nearest_break);
				if(expList == NULL) {
					expList = tail = Tr_ExpList(exp.exp, NULL);
				} else {
					tail->tail = Tr_ExpList(exp.exp, NULL);
					tail = tail->tail;
				}
				seq = seq->tail;
			}
			return expTy(Tr_seqExp(expList), exp.ty);
		}
		case A_assignExp: {
			struct expty lvalue = transVar(venv, tenv, get_assexp_var(a), l, nearest_break);
			struct expty exp = transExp(venv, tenv, get_assexp_exp(a), l, nearest_break);
			hasLoopVar(venv, get_assexp_var(a));
			if(actual_ty(lvalue.ty) != actual_ty(exp.ty)) {
				EM_error(a->pos, "unmatched assign exp");
			}
			return expTy(Tr_assignExp(lvalue.exp, exp.exp), Ty_Void());
		}
		case A_ifExp: {
			transExp(venv, tenv, get_ifexp_test(a), l, nearest_break);
			struct expty ei = transExp(venv, tenv, get_ifexp_test(a), l, nearest_break);
			struct expty et = transExp(venv, tenv, get_ifexp_then(a), l, nearest_break);
			struct expty ee = transExp(venv, tenv, get_ifexp_else(a), l, nearest_break);
			if(get_expty_kind(ee) == Ty_nil) {
				if(get_expty_kind(et) != Ty_void) {
					EM_error(a->pos, "if-then exp's body must produce no value");
				}
				return expTy(Tr_ifExp(ei.exp, et.exp, NULL), Ty_Void());
			} else {
				if(ee.ty != et.ty) {
					EM_error(a->pos, "then exp and else exp type mismatch");
				}
				return expTy(Tr_ifExp(ei.exp, et.exp, ee.exp), et.ty);
			}
		}
		case A_whileExp: {
			struct expty test = transExp(venv, tenv, get_whileexp_test(a), l, nearest_break);
			Temp_label done = Temp_newlabel();
			struct expty body = transExp(venv, tenv, get_whileexp_body(a), l, done);	
			if(actual_ty(test.ty)->kind != Ty_int) {
				EM_error(a->u.whilee.test->pos, "type of test expression shoulf be int");
			}
			if(actual_ty(body.ty)->kind != Ty_void) {
				EM_error(a->u.whilee.body->pos, "while body must produce no value");
			}
			return expTy(Tr_whileExp(test.exp, body.exp, done), Ty_Void());
		}
		case A_forExp: {
			struct expty start = transExp(venv, tenv, get_forexp_lo(a), l, nearest_break);
			struct expty end = transExp(venv, tenv, get_forexp_hi(a), l, nearest_break);
			if(actual_ty(start.ty)->kind != Ty_int) {
				EM_error(get_forexp_lo(a)->pos, "for exp's range type is not integer");
			}
			if(actual_ty(end.ty)->kind != Ty_int) {
				EM_error(get_forexp_hi(a)->pos, "for exp's range type is not integer");
			}
			Tr_access ta = Tr_allocLocal(l, TRUE);
			S_enter(venv, get_forexp_var(a), E_ROVarEntry(ta, Ty_Int()));
			S_beginScope(venv);
			Temp_label done = Temp_newlabel();
			struct expty body = transExp(venv, tenv, get_forexp_body(a), l, done);
			S_endScope(venv);
			if(actual_ty(body.ty)->kind != Ty_void) {
				EM_error(get_forexp_body(a)->pos, "type of body expression should be void");
			}
			return expTy(Tr_forExp(ta, start.exp, end.exp, body.exp, done), Ty_Void());
		}
		case A_breakExp: {
			if (!nearest_break) {
				return expTy(Tr_noExp(), Ty_Void());
			}
			return expTy(Tr_breakExp(nearest_break), Ty_Void());
		}
		case A_letExp: {
			S_beginScope(venv);
			S_beginScope(tenv);
			A_decList d;
			Tr_expList exps = NULL;
			Tr_expList tail = NULL;
			for(d = get_letexp_decs(a); d; d = d->tail) {
				if (exps == NULL) {
					exps = tail = Tr_ExpList(transDec(venv, tenv, d->head, l, nearest_break), NULL);
				} else {
					tail->tail = Tr_ExpList(transDec(venv, tenv, d->head, l, nearest_break), NULL);
					tail = tail->tail;
				}
			}
		    struct expty exp = transExp(venv, tenv, get_letexp_body(a), l, nearest_break);
		    tail->tail = Tr_ExpList(exp.exp, NULL);
			S_endScope(tenv);
		    S_endScope(venv);
			return expTy(Tr_seqExp(exps), exp.ty);
		}
		case A_arrayExp: {
			Ty_ty type = S_look(tenv, get_arrayexp_typ(a));
			type = actual_ty(type);
			if(!type) {
				EM_error(a->pos, "undefined type %s", S_name(get_arrayexp_typ(a)));
				return expTy(Tr_noExp(), Ty_Int());
			}
			if(type->kind != Ty_array) {
				EM_error(a->pos, "not array type %s", S_name(get_recordexp_typ(a)));
				return expTy(Tr_noExp(), type);
			}
			struct expty size = transExp(venv, tenv, get_arrayexp_size(a), l, nearest_break);
			struct expty init = transExp(venv, tenv, get_arrayexp_init(a), l, nearest_break);
			if(actual_ty(size.ty)->kind != Ty_int) {
				EM_error(get_arrayexp_size(a)->pos, "type of size expression should be int");
			}
			if(actual_ty(init.ty) != actual_ty(get_ty_array(type))) {
				EM_error(get_arrayexp_init(a)->pos, "type mismatch");
			}
			return expTy(Tr_arrayExp(size.exp, init.exp), type);
		}
	}
	assert(0); /* should have returned from some clause of the switch */
}

Ty_tyList makeFormalTyList(S_table tenv, A_fieldList params) {
	if (params == NULL) {
		return NULL;
	}
	Ty_ty type = S_look(tenv, params->head->typ);
	if(type == NULL) {
		EM_error(params->head->pos, "undefined type %s", params->head->typ);
		type = Ty_Int();
	}
	return Ty_TyList(type, makeFormalTyList(tenv, params->tail));
}

U_boolList makeFormalBoolList(A_fieldList fieldList)
{
	A_fieldList f;
	U_boolList head = NULL;
	U_boolList tail = NULL;
	for (f = fieldList; f; f=f->tail) {
		bool escape = f->head->escape;
		if (head == NULL) {
			head = tail = U_BoolList(escape, NULL);
		} else {
			tail->tail = U_BoolList(escape, NULL);
			tail = tail->tail;
		}
	}
	return head;
}


Tr_exp transDec(S_table venv, S_table tenv, A_dec d, Tr_level l, Temp_label nearest_break) {
	switch(d->kind) {
		case A_functionDec: {
			A_fundecList fl, ffl;
			A_fundec f;
			Ty_ty resultTy;
			Ty_tyList formalTys;
			U_boolList formalEscapes;
			Tr_level newLevel;
			Temp_label newLabel;
			/* 
			 * The first iteration
			 * 1. Check duplicates
			 * 2. Check result type
			 * 3. Build formal type list
			 * 4. Build formal escape list
			 * 5. Allocate new frame
			 * 6. Enter the FunEntry
			 */
			for (fl = get_funcdec_list(d); fl; fl=fl->tail) {
				f = fl->head;
				for (ffl = get_funcdec_list(d); ffl != fl; ffl=ffl->tail) {
					if (ffl->head->name == f->name) {
						EM_error(f->pos, "two functions have the same name");
						break;
					}
				}
				if (f->result) {
					resultTy = S_look(tenv, f->result);
				} else {
					resultTy = Ty_Void();
				}
				formalTys = makeFormalTyList(tenv, f->params);
				formalEscapes = makeFormalBoolList(f->params);
				newLabel = Temp_newlabel();
				newLevel = Tr_newLevel(l, newLabel, formalEscapes);
				S_enter(venv, f->name, E_FunEntry(newLevel, newLabel, formalTys, resultTy));
			}
			/*
			 * The second iteration
			 * 1. Enter the VarEntry of formals
			 * 2. Transverse the body
			 * 3. Check return type
			 */ 
			for (fl = get_funcdec_list(d); fl; fl=fl->tail) {
				f = fl->head;
				E_enventry x = S_look(venv, f->name);
				S_beginScope(venv);
				A_fieldList l;
				Ty_tyList t;
				Tr_accessList al = Tr_formals(get_func_level(x));
				for (l = f->params, t = get_func_tylist(x); l; l=l->tail, t=t->tail, al=al->tail) {
					S_enter(venv, l->head->name, E_VarEntry(al->head, t->head));
				}

				struct expty e = transExp(venv, tenv, f->body, get_func_level(x), nearest_break);

				if (get_func_res(x)->kind == Ty_void && get_expty_kind(e) != Ty_void) {
					EM_error(f->pos, "procedure returns value");
				}
				/* TODO: other return type check */
				Tr_procEntryExit(get_func_level(x), e.exp, al);
				S_endScope(venv);
			}
			return Tr_noExp();
		}
		case A_varDec: {
			struct expty init = transExp(venv, tenv, get_vardec_init(d), l, nearest_break);
			Tr_access a = Tr_allocLocal(l, d->u.var.escape);
			if(get_vardec_typ(d) != NULL) {
				Ty_ty type = S_look(tenv, get_vardec_typ(d));
				if(type == NULL) {
					EM_error(get_vardec_init(d)->pos, "type not exist %s", S_name(get_vardec_typ(d)));
				}
				if(actual_ty(type) != actual_ty(init.ty)) {
					EM_error(get_vardec_init(d)->pos, "type mismatch");
				}
			} else if(actual_ty(init.ty)->kind == Ty_nil) {
				EM_error(get_vardec_init(d)->pos, "init should not be nil without type specified");
				S_enter(venv, get_vardec_var(d), E_VarEntry(a, Ty_Int()));
			}
			S_enter(venv, get_vardec_var(d), E_VarEntry(a, init.ty));
			return Tr_assignExp(Tr_simpleVar(a, l), init.exp);
		}
		case A_typeDec: {
			A_nametyList types = get_typedec_list(d);
			while(types != NULL) {
				if (S_look(tenv, types->head->name) != NULL) {
					EM_error(d->pos, "two types have the same name");
				} else {
					S_enter(tenv, types->head->name, Ty_Name(types->head->name, NULL));
				}
				types = types->tail;
			}

			// Resolve references
			types = d->u.type;
			while(types != NULL) {
				Ty_ty type = S_look(tenv, types->head->name);
				get_ty_name(type).ty = transTy(tenv, types->head->ty);
				types = types->tail;
			}

			// Cycle detection
			types = get_typedec_list(d);
			while(types != NULL) {
				Ty_ty init = S_look(tenv, types->head->name);
				Ty_ty type = init;
				while((type = get_ty_name(type).ty)->kind == Ty_name) {
					if (type == init) {
						EM_error(d->pos, "illegal type cycle");
						get_ty_name(init).ty = Ty_Int();
						break;
					}
				}
				types = types->tail;
			}

			return Tr_noExp();
		}
	}
}

Ty_fieldList makeFieldList(S_table tenv, A_fieldList fields) {
	Ty_ty type = S_look(tenv, fields->head->typ);
	if(type == NULL) {
		EM_error(fields->head->pos, "undefined type %s", S_name(fields->head->typ));
		type = Ty_Int();
	}
	Ty_field field = Ty_Field(fields->head->name, type);
	if(fields->tail == NULL) {
		return Ty_FieldList(field, NULL);
	}else {
		return Ty_FieldList(field, makeFieldList(tenv, fields->tail));
	}
}

Ty_ty transTy (S_table tenv, A_ty a) {
	switch(a->kind) {
		case A_nameTy: {
			Ty_ty type = S_look(tenv, get_ty_name(a));
			if(type == NULL) {
				EM_error(a->pos, "undefined type %s", S_name(get_ty_name(a)));
				return Ty_Int();
			}
			return Ty_Name(get_ty_name(a), type);
		}
		case A_recordTy: {
			return Ty_Record(makeFieldList(tenv, get_ty_record(a)));
		}
		case A_arrayTy: {
			Ty_ty type = S_look(tenv, get_ty_array(a));
			if(type == NULL) {
				EM_error(a->pos, "undefined type %s", S_name(get_ty_array(a)));
				return Ty_Array(Ty_Int());
			}
			return Ty_Array(type);
		}
	}
	return NULL;
}


F_fragList SEM_transProg(A_exp exp){

	//TODO LAB5: do not forget to add the main frame
	F_FP();
	S_table t = E_base_tenv();
	S_table v = E_base_venv();
	Temp_label mainLabel = Temp_newlabel();
	Tr_level mainLevel = Tr_newLevel(Tr_outermost(), mainLabel, NULL);
	struct expty main = transExp(v, t, exp, mainLevel, NULL);
	Tr_procEntryExit(mainLevel, main.exp, NULL);
	Tr_print(main.exp);
	return Tr_getResult();
}

