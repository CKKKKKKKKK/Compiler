#include <stdio.h>
#include <assert.h>
#include <string.h>
#include "util.h"
#include "errormsg.h"
#include "symbol.h"
#include "absyn.h"
#include "types.h"
#include "helper.h"
#include "env.h"
#include "semant.h"

/*Lab4: Your implementation of lab4*/


typedef void* Tr_exp;
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
	while(t && t->kind == Ty_name) {
		t = get_ty_name(t).ty;
	}
	return t;
}

struct expty transVar(S_table venv, S_table tenv, A_var v) 
{
	switch(v->kind) {
		case A_simpleVar: {
			E_enventry x = S_look(venv, get_simplevar_sym(v));
			if(!x || x->kind != E_varEntry) {
				EM_error(v->pos, "undefined variable %s", S_name(get_simplevar_sym(v)));
				return expTy(NULL, Ty_Int());
			}
			return expTy(NULL, actual_ty(get_varentry_type(x)));
		}
	   	case A_fieldVar: {
			struct expty lvalue = transVar(venv, tenv, get_fieldvar_var(v));
			if(get_expty_kind(lvalue) != Ty_record) {
				EM_error(v->pos, "not a record type");
				return expTy(NULL, Ty_Int());
			}

			// Find the field of the record
			Ty_fieldList fields = get_record_fieldlist(lvalue);
			while(fields && fields->head->name != get_fieldvar_sym(v)) {
				fields = fields->tail;
			}
			if(fields == NULL) {
				EM_error(v->pos, "field %s doesn't exist", S_name(get_fieldvar_sym(v)));
				return expTy(NULL, Ty_Int());
			}

			return expTy(NULL, actual_ty(fields->head->ty));
		}
		case A_subscriptVar: {
			struct expty lvalue = transVar(venv, tenv, get_subvar_var(v));
			struct expty index = transExp(venv, tenv, get_subvar_exp(v));
			if(get_expty_kind(lvalue) != Ty_array) {
				EM_error(v->pos, "array type required");
				return expTy(NULL, Ty_Int());
			}
			if(get_expty_kind(index) != Ty_int) {
				EM_error(v->pos, "index type is not int");
				return expTy(NULL, Ty_Int());
			}
			return expTy(NULL, actual_ty(get_array(lvalue)));
		}
	}
	assert(0); /* should have returned from some clause of the switch */
}

int hasLoopVar(S_table venv, A_var v) 
{
	switch(v->kind) {
		case A_simpleVar: {
			E_enventry x = S_look(venv, get_simplevar_sym(v));
			if(x->readonly) {
				EM_error(v->pos, "loop variable can't be assigned");
				return 1;
			}
			return 0;
		}
	   	case A_fieldVar: {
			return hasLoopVar(venv, get_fieldvar_var(v));
		}			
		case A_subscriptVar: {
			return hasLoopVar(venv, get_subvar_var(v));
		}
	}
	assert(0); /* should have returned from some clause of the switch */
}

struct expty transExp(S_table venv, S_table tenv, A_exp a)
{
	switch(a->kind) {
		case A_varExp: {
			return transVar(venv, tenv, a->u.var);
		}
		case A_nilExp: {
			return expTy(NULL, Ty_Nil());
		}
		case A_intExp: {
			return expTy(NULL, Ty_Int());
		}
		case A_stringExp: {
			return expTy(NULL, Ty_String());
		}
		case A_callExp: {
			E_enventry x = S_look(venv, get_callexp_func(a));
			if(!x || x->kind != E_funEntry) {
				EM_error(a->pos, "undefined function %s", S_name(get_callexp_func(a)));
				return expTy(NULL, Ty_Int());
			}

			// Check parameter types and number
			Ty_tyList expected = get_func_tylist(x);
			A_expList actual = get_callexp_args(a);
			for( ; actual && expected; actual = actual->tail, expected = expected->tail) {
				struct expty exp = transExp(venv, tenv, actual->head);
				if(actual_ty(exp.ty)->kind != actual_ty(expected->head)->kind) {
					EM_error(actual->head->pos, "para type mismatch");
				}
			}
			if(expected != NULL || actual != NULL) {
				EM_error(a->pos, "too many params in function %s", S_name(get_callexp_func(a)));
			}

			return expTy(NULL, actual_ty(get_func_res(x)));
		}
		case A_opExp: {
			A_oper oper = get_opexp_oper(a);
			struct expty left = transExp(venv, tenv, get_opexp_left(a));
			struct expty right = transExp(venv, tenv, get_opexp_right(a));
			if(oper == A_plusOp || oper == A_minusOp || oper == A_timesOp || oper == A_divideOp) {
				if(actual_ty(left.ty)->kind != Ty_int) {
					EM_error(get_opexp_leftpos(a), "integer required");
					return expTy(NULL, Ty_Int());
				}
				if(actual_ty(right.ty)->kind != Ty_int) {
					EM_error(get_opexp_rightpos(a), "integer required");
					return expTy(NULL, Ty_Int());
				}
				return expTy(NULL, Ty_Int());
			} else {
				if(actual_ty(left.ty) != actual_ty(right.ty)) {
					EM_error(get_opexp_leftpos(a), "same type required");
				}
			}
			return expTy(NULL, Ty_Int());
		}
		case A_recordExp: {
			Ty_ty type = S_look(tenv, get_recordexp_typ(a));
			type = actual_ty(type);
			if(type == NULL) {
				EM_error(a->pos, "undefined type %s", S_name(get_recordexp_typ(a)));
				return expTy(NULL, Ty_Int());
			}
			if(type->kind !=Ty_record) {
				EM_error(a->pos, "not record type %s", S_name(get_recordexp_typ(a)));
				return expTy(NULL, type);
			}

			// Check fields
			Ty_fieldList expected = type->u.record;
			A_efieldList actual = get_recordexp_fields(a);
			for( ; actual && expected; actual = actual->tail, expected = expected->tail) {
				// Check name and type
				if(expected->head->name != actual->head->name) {
					EM_error(a->pos, "expected %s but get %s", S_name(expected->head->name), S_name(actual->head->name));
				}
				struct expty exp = transExp(venv, tenv, actual->head->exp);
				if(actual_ty(expected->head->ty) != actual_ty(exp.ty)) {
					EM_error(a->pos, "type not match");
				}
			}
			if(expected != NULL || actual != NULL) {
				EM_error(a->pos, "field number of %s does not match", S_name(a->u.record.typ));
			}

			return expTy(NULL, type);
		}
		case A_seqExp: {
			A_expList seq = get_seqexp_seq(a);
			struct expty exp = expTy(NULL, Ty_Void());
			while(seq != NULL) {
				exp = transExp(venv, tenv, seq->head);
				seq = seq->tail;
			}
			return exp;
		}
		case A_assignExp: {
			struct expty lvalue = transVar(venv, tenv, get_assexp_var(a));
			struct expty exp = transExp(venv, tenv, get_assexp_exp(a));
			hasLoopVar(venv, get_assexp_var(a));
			if(actual_ty(lvalue.ty) != actual_ty(exp.ty)) {
				EM_error(a->pos, "unmatched assign exp");
			}
			return expTy(NULL, Ty_Void());
		}
		case A_ifExp: {
			transExp(venv, tenv, get_ifexp_test(a));
			struct expty et = transExp(venv, tenv, get_ifexp_then(a));
			struct expty ee = transExp(venv, tenv, get_ifexp_else(a));
			if(get_expty_kind(ee) == Ty_nil) {
				if(get_expty_kind(et) != Ty_void) {
					EM_error(a->pos, "if-then exp's body must produce no value");
				}
				return expTy(NULL, Ty_Void());
			} else {
				if(ee.ty != et.ty) {
					EM_error(a->pos, "then exp and else exp type mismatch");
				}
				return et;
			}
		}
		case A_whileExp: {
			struct expty test = transExp(venv, tenv, get_whileexp_test(a));
			struct expty body = transExp(venv, tenv, get_whileexp_body(a));
			if(actual_ty(test.ty)->kind != Ty_int) {
				EM_error(a->u.whilee.test->pos, "type of test expression shoulf be int");
			}
			if(actual_ty(body.ty)->kind != Ty_void) {
				EM_error(a->u.whilee.body->pos, "while body must produce no value");
			}
			return expTy(NULL, Ty_Void());
		}
		case A_forExp: {
			struct expty start = transExp(venv, tenv, get_forexp_lo(a));
			struct expty end = transExp(venv, tenv, get_forexp_hi(a));
			if(actual_ty(start.ty)->kind != Ty_int) {
				EM_error(get_forexp_lo(a)->pos, "for exp's range type is not integer");
			}
			if(actual_ty(end.ty)->kind != Ty_int) {
				EM_error(get_forexp_hi(a)->pos, "for exp's range type is not integer");
			}
			S_enter(venv, get_forexp_var(a), E_ROVarEntry(Ty_Int()));
			S_beginScope(venv);
			struct expty body = transExp(venv, tenv, get_forexp_body(a));
			S_endScope(venv);
			if(actual_ty(body.ty)->kind != Ty_void) {
				EM_error(get_forexp_body(a)->pos, "type of body expression should be void");
			}
			return expTy(NULL, Ty_Void());
		}
		case A_breakExp: {
			return expTy(NULL, Ty_Void());
		}
		case A_letExp: {
			S_beginScope(venv);
			S_beginScope(tenv);
			A_decList d;
			for(d = a->u.let.decs; d; d = d->tail) {
				transDec(venv, tenv, d->head);
			}
		    struct expty exp = transExp(venv, tenv, get_letexp_body(a));
		    S_endScope(tenv);
		    S_endScope(venv);
			return exp;
		}
		case A_arrayExp: {
			Ty_ty type = S_look(tenv, get_arrayexp_typ(a));
			type = actual_ty(type);
			if(!type) {
				EM_error(a->pos, "undefined type %s", S_name(get_arrayexp_typ(a)));
				return expTy(NULL, Ty_Int());
			}
			if(type->kind != Ty_array) {
				EM_error(a->pos, "not array type %s", S_name(get_recordexp_typ(a)));
				return expTy(NULL, type);
			}
			struct expty size = transExp(venv, tenv, get_arrayexp_size(a));
			struct expty init = transExp(venv, tenv, get_arrayexp_init(a));
			if(actual_ty(size.ty)->kind != Ty_int) {
				EM_error(get_arrayexp_size(a)->pos, "type of size expression should be int");
			}
			if(actual_ty(init.ty) != actual_ty(get_ty_array(type))) {
				EM_error(get_arrayexp_init(a)->pos, "type mismatch");
			}
			return expTy(NULL, type);
		}
	}
	assert(0); /* should have returned from some clause of the switch */
}

Ty_tyList makeFormalTyList(S_table tenv, A_fieldList params) {
	if (params == NULL) {
		return NULL;
	}
	Ty_ty type = S_look(tenv, params->head->typ);
	return Ty_TyList(type, makeFormalTyList(tenv, params->tail));
}

void transDec(S_table venv, S_table tenv, A_dec d) {
	switch(d->kind) {
		case A_functionDec: {
			A_fundecList func = get_funcdec_list(d);
			while(func) {
				if(S_look(venv, func->head->name) != NULL) {
					EM_error(d->pos, "two functions have the same name");
					func = func->tail;
					continue;
				}
				Ty_ty resultTy;
				if(func->head->result) {
					resultTy = S_look(tenv, func->head->result);
					if(!resultTy) {
						EM_error(func->head->pos, "undefined result type %s", S_name(func->head->result));
						resultTy = Ty_Void();
					}
				}else {
					resultTy = Ty_Void();
				}
				Ty_tyList formalTys = makeFormalTyList(tenv, func->head->params);
				E_enventry entry = E_FunEntry(formalTys, resultTy);
				entry->kind = E_funEntry;
				S_enter(venv, func->head->name, entry);
				func = func->tail;
			}

			// Check function bodies
			func = d->u.function;
			while(func) {
				Ty_tyList formalTys = makeFormalTyList(tenv,func->head->params);
				A_fieldList formalNames = func->head->params;
				S_beginScope(venv);
				for( ; formalNames; formalNames = formalNames->tail, formalTys = formalTys->tail) {
					S_enter(venv, formalNames->head->name, E_VarEntry(formalTys->head));
				}
				struct expty exp = transExp(venv, tenv, func->head->body);
				E_enventry x = S_look(venv, func->head->name);
				// Check return type
				if(actual_ty(get_func_res(x))->kind == Ty_void) {
					if(actual_ty(exp.ty)->kind != Ty_void) {
						EM_error(func->head->pos, "procedure returns value");
					}
				}else if(actual_ty(get_func_res(x))->kind != actual_ty(exp.ty)->kind) {
					EM_error(func->head->pos, "procedure returns unexpected type");
				}
				S_endScope(venv);
				func = func->tail;
			}

			break;
		}
		case A_varDec: {
			struct expty init = transExp(venv, tenv, get_vardec_init(d));
			if(get_vardec_typ(d) != NULL) {
				Ty_ty type = S_look(tenv, get_vardec_typ(d));
				if(type == NULL) {
					EM_error(get_vardec_init(d)->pos, "type not exist %s", S_name(get_vardec_typ(d)));
				}
				if(actual_ty(type) != actual_ty(init.ty)) {
					EM_error(get_vardec_init(d)->pos, "type mismatch");
				}
			}else if(actual_ty(init.ty)->kind == Ty_nil) {
				EM_error(get_vardec_init(d)->pos, "init should not be nil without type specified");
			}
			S_enter(venv, get_vardec_var(d), E_VarEntry(init.ty));
			break;
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

			break;
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
				return Ty_Int();
			}
			return Ty_Array(type);
		}
	}
	return NULL;
}

void SEM_transProg(A_exp exp) {
	transExp(E_base_venv(), E_base_tenv(), exp);
}
