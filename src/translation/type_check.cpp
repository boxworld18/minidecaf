/*****************************************************
 *  Implementation of the second semantic analysis pass.
 *
 *  In the second pass, we will check:
 *    1. whether all the expressions are well-typed; (and sets ATTR(type))
 *    2. whether all the statements are well-formed;
 *    3. whether all the referenced symbols are well-defined. (and sets
 * ATTR(sym))
 *
 *  Keltin Leung 
 */

#include "3rdparty/list.hpp"
#include "ast/ast.hpp"
#include "ast/visitor.hpp"
#include "compiler.hpp"
#include "config.hpp"
#include "scope/scope_stack.hpp"
#include "symb/symbol.hpp"
#include "type/type.hpp"

using namespace mind;
using namespace mind::type;
using namespace mind::scope;
using namespace mind::symb;
using namespace mind::util;
using namespace mind::err;

/* Pass 2 of the semantic analysis.
 */
class SemPass2 : public ast::Visitor {
    // Visiting expressions
    virtual void visit(ast::AssignExpr *);
    // Step3
    virtual void visit(ast::AddExpr *);
    virtual void visit(ast::SubExpr *);
    virtual void visit(ast::MulExpr *);
    virtual void visit(ast::DivExpr *);
    virtual void visit(ast::ModExpr *);

    virtual void visit(ast::IntConst *);
    // Step2
    virtual void visit(ast::NegExpr *);
    virtual void visit(ast::NotExpr *);
    virtual void visit(ast::BitNotExpr *);

    // Step4
    virtual void visit(ast::EquExpr *);
    virtual void visit(ast::NeqExpr *);
    virtual void visit(ast::AndExpr *);
    virtual void visit(ast::OrExpr *);
    virtual void visit(ast::GeqExpr *);
    virtual void visit(ast::LeqExpr *);
    virtual void visit(ast::LesExpr *);
    virtual void visit(ast::GrtExpr *);

    // Step6
    virtual void visit(ast::IfExpr *);

    virtual void visit(ast::LvalueExpr *);
    virtual void visit(ast::VarRef *);
    // Visiting statements
    virtual void visit(ast::VarDecl *);
    virtual void visit(ast::CompStmt *);
    virtual void visit(ast::ExprStmt *);
    virtual void visit(ast::IfStmt *);
    virtual void visit(ast::ReturnStmt *);
    virtual void visit(ast::WhileStmt *);

    // Step8
    virtual void visit(ast::ForStmt *);
    virtual void visit(ast::DoWhileStmt *);

    // Step9
    virtual void visit(ast::CallExpr *);

    // Step11
    virtual void visit(ast::ArrayRef *);
    virtual void visit(ast::IndexExpr *);

    // Visiting declarations
    virtual void visit(ast::FuncDefn *);
    virtual void visit(ast::Program *);
};

// recording the current return type
static Type *retType = NULL;
// recording the current "this" type

/* Determines whether a given type is BaseType::Error.
 *
 * NOTE:
 *   don't use the == operator when comparing types
 * PARAMETERS:
 *   t     - the type to check
 */
static bool isErrorType(Type *t) { return t->equal(BaseType::Error); }

/* Checks whether an ast::Expr conforms to the expecting type.
 *
 * NOTE:
 *   if the expression type is BaseType::Error, we accept it as a legal case.
 * PARAMETERS:
 *   e     - the ast::Expr node
 *   t     - the expected type
 * SIDE-EFFECTS:
 *   Unexpected Type Error may be issued
 */
static void expect(ast::Expr *e, Type *t) {
    if (!e->ATTR(type)->equal(t) && !isErrorType(e->ATTR(type))) {
        issue(e->getLocation(), new UnexpectedTypeError(t, e->ATTR(type)));
    }
}

/* Visits an ast::IntConst node.
 *
 * PARAMETERS:
 *   e     - the ast::IntConst node
 */
void SemPass2::visit(ast::IntConst *e) { e->ATTR(type) = BaseType::Int; }

/* Visits an ast::AddExpr node.
 *
 * PARAMETERS:
 *   e     - the ast::AddExpr node
 */
void SemPass2::visit(ast::AddExpr *e) {
    e->e1->accept(this);
    expect(e->e1, BaseType::Int);

    e->e2->accept(this);
    expect(e->e2, BaseType::Int);

    e->ATTR(type) = BaseType::Int;
}

/* Step3 started */

/* Visits an ast::SubExpr node.
 *
 * PARAMETERS:
 *   e     - the ast::SubExpr node
 */
void SemPass2::visit(ast::SubExpr *e) {
    e->e1->accept(this);
    expect(e->e1, BaseType::Int);

    e->e2->accept(this);
    expect(e->e2, BaseType::Int);

    e->ATTR(type) = BaseType::Int;
}

/* Visits an ast::MulExpr node.
 *
 * PARAMETERS:
 *   e     - the ast::MulExpr node
 */
void SemPass2::visit(ast::MulExpr *e) {
    e->e1->accept(this);
    expect(e->e1, BaseType::Int);

    e->e2->accept(this);
    expect(e->e2, BaseType::Int);

    e->ATTR(type) = BaseType::Int;
}

/* Visits an ast::DivExpr node.
 *
 * PARAMETERS:
 *   e     - the ast::DivExpr node
 */
void SemPass2::visit(ast::DivExpr *e) {
    e->e1->accept(this);
    expect(e->e1, BaseType::Int);

    e->e2->accept(this);
    expect(e->e2, BaseType::Int);

    e->ATTR(type) = BaseType::Int;
}

/* Visits an ast::ModExpr node.
 *
 * PARAMETERS:
 *   e     - the ast::ModExpr node
 */
void SemPass2::visit(ast::ModExpr *e) {
    e->e1->accept(this);
    expect(e->e1, BaseType::Int);

    e->e2->accept(this);
    expect(e->e2, BaseType::Int);

    e->ATTR(type) = BaseType::Int;
}

/* Step3 ended */

/* Visits an ast::NegExpr node.
 *
 * PARAMETERS:
 *   e     - the ast::NegExpr node
 */
void SemPass2::visit(ast::NegExpr *e) {
    e->e->accept(this);
    expect(e->e, BaseType::Int);

    e->ATTR(type) = BaseType::Int;
}

/* Step4 started */

/* Visits an ast::EquExpr node.
 *
 * PARAMETERS:
 *   e     - the ast::EquExpr node
 */
void SemPass2::visit(ast::EquExpr *e) {
    e->e1->accept(this);
    expect(e->e1, BaseType::Int);

    e->e2->accept(this);
    expect(e->e2, BaseType::Int);

    e->ATTR(type) = BaseType::Int;
}

/* Visits an ast::NeqExpr node.
 *
 * PARAMETERS:
 *   e     - the ast::NeqExpr node
 */
void SemPass2::visit(ast::NeqExpr *e) {
    e->e1->accept(this);
    expect(e->e1, BaseType::Int);

    e->e2->accept(this);
    expect(e->e2, BaseType::Int);

    e->ATTR(type) = BaseType::Int;
}

/* Visits an ast::AndExpr node.
 *
 * PARAMETERS:
 *   e     - the ast::AndExpr node
 */
void SemPass2::visit(ast::AndExpr *e) {
    e->e1->accept(this);
    expect(e->e1, BaseType::Int);

    e->e2->accept(this);
    expect(e->e2, BaseType::Int);

    e->ATTR(type) = BaseType::Int;
}

/* Visits an ast::OrExpr node.
 *
 * PARAMETERS:
 *   e     - the ast::OrExpr node
 */
void SemPass2::visit(ast::OrExpr *e) {
    e->e1->accept(this);
    expect(e->e1, BaseType::Int);

    e->e2->accept(this);
    expect(e->e2, BaseType::Int);

    e->ATTR(type) = BaseType::Int;
}

/* Visits an ast::GeqExpr node.
 *
 * PARAMETERS:
 *   e     - the ast::GeqExpr node
 */
void SemPass2::visit(ast::GeqExpr *e) {
    e->e1->accept(this);
    expect(e->e1, BaseType::Int);

    e->e2->accept(this);
    expect(e->e2, BaseType::Int);

    e->ATTR(type) = BaseType::Int;
}

/* Visits an ast::LeqExpr node.
 *
 * PARAMETERS:
 *   e     - the ast::LeqExpr node
 */
void SemPass2::visit(ast::LeqExpr *e) {
    e->e1->accept(this);
    expect(e->e1, BaseType::Int);

    e->e2->accept(this);
    expect(e->e2, BaseType::Int);

    e->ATTR(type) = BaseType::Int;
}

/* Visits an ast::LesExpr node.
 *
 * PARAMETERS:
 *   e     - the ast::LesExpr node
 */
void SemPass2::visit(ast::LesExpr *e) {
    e->e1->accept(this);
    expect(e->e1, BaseType::Int);

    e->e2->accept(this);
    expect(e->e2, BaseType::Int);

    e->ATTR(type) = BaseType::Int;
}

/* Visits an ast::GrtExpr node.
 *
 * PARAMETERS:
 *   e     - the ast::GrtExpr node
 */
void SemPass2::visit(ast::GrtExpr *e) {
    e->e1->accept(this);
    expect(e->e1, BaseType::Int);

    e->e2->accept(this);
    expect(e->e2, BaseType::Int);

    e->ATTR(type) = BaseType::Int;
}

/* Step4 ended */

/* Step2 started */

/* Visits an ast::NotExpr node.
 *
 * PARAMETERS:
 *   e     - the ast::NotExpr node
 */
void SemPass2::visit(ast::NotExpr *e) {
    e->e->accept(this);
    expect(e->e, BaseType::Int);

    e->ATTR(type) = BaseType::Int;
}

/* Visits an ast::BitNotExpr node.
 *
 * PARAMETERS:
 *   e     - the ast::BitNotExpr node
 */
void SemPass2::visit(ast::BitNotExpr *e) {
    e->e->accept(this);
    expect(e->e, BaseType::Int);

    e->ATTR(type) = BaseType::Int;
}

/* Step2 ended */

/* Visits an ast::LvalueExpr node.
 *
 * PARAMETERS:
 *   e     - the ast::LvalueExpr node
 */
void SemPass2::visit(ast::LvalueExpr *e) {
    e->lvalue->accept(this);
    e->ATTR(type) = e->lvalue->ATTR(type);
}

/* Visits an ast::VarRef node.
 *
 * PARAMETERS:
 *   e     - the ast::VarRef node
 */
void SemPass2::visit(ast::VarRef *ref) {
    // CASE I: owner is NULL ==> referencing a local var or a member var?
    Symbol *v = scopes->lookup(ref->var, ref->getLocation());
    if (NULL == v) {
        issue(ref->getLocation(), new SymbolNotFoundError(ref->var));
        goto issue_error_type;

    } else if (!v->isVariable()) {
        issue(ref->getLocation(), new NotVariableError(v));
        goto issue_error_type;

    } else {
        ref->ATTR(type) = v->getType();
        ref->ATTR(sym) = (Variable *)v;

        if (ref->ATTR(sym)->isLocalVar())
            ref->ATTR(lv_kind) = ast::Lvalue::SIMPLE_VAR;

        // if (ref->ATTR(sym)->getType()->isArrayType())
        //     ref->ATTR(lv_kind) = ast::Lvalue::ARRAY_ELE;
    }

    return;

    // sometimes "GOTO" will make things simpler. this is one of such cases:
issue_error_type:
    ref->ATTR(type) = BaseType::Error;
    ref->ATTR(sym) = NULL;
    return;
}

/* Visits an ast::ArrayRef node.
 *
 * PARAMETERS:
 *   e     - the ast::ArrayRef node
 */
void SemPass2::visit(ast::ArrayRef *ref) {
    // CASE I: owner is NULL ==> referencing a local var or a member var?
    Symbol *v = scopes->lookup(ref->var, ref->getLocation());
    if (NULL == v) {
        issue(ref->getLocation(), new SymbolNotFoundError(ref->var));
        goto issue_error_type;

    } else if (!v->isVariable()) {
        issue(ref->getLocation(), new NotVariableError(v));
        goto issue_error_type;

    } else if (!v->getType()->isArrayType()) {
        issue(ref->getLocation(), new NotArrayError());
        goto issue_error_type;

    } else if (ref->index == NULL) {
        issue(ref->getLocation(), new NotArrayError());
        goto issue_error_type;

    } else {
        ref->index->accept(this);

        Type *atype = v->getType();
        ast::IndexExpr *iexpr = ref->index;
        
        while (iexpr != NULL) {
            iexpr->ATTR(type) = atype;
            if (iexpr->expr->ATTR(type) != BaseType::Int) {
                issue(ref->getLocation(), new UnexpectedTypeError(iexpr->expr->ATTR(type), BaseType::Int));
                goto issue_error_type;
            }
            iexpr = iexpr->index;
            if (atype->isArrayType()) {
                atype = ((ArrayType*) atype)->getElementType();
            }
            
            if (atype->equal(BaseType::Int) && iexpr != NULL) {
                issue(ref->getLocation(), new DeclConflictError(ref->var, v));
                goto issue_error_type;
            }
        }

        if (!atype->equal(BaseType::Int)) {
            issue(ref->getLocation(), new DeclConflictError(ref->var, v));
            goto issue_error_type;
        }

        ref->ATTR(type) = BaseType::Int;
        ref->ATTR(sym) = (Variable *)v;
        ref->ATTR(lv_kind) = ast::Lvalue::ARRAY_ELE;
        
    }

    return;

    // sometimes "GOTO" will make things simpler. this is one of such cases:
issue_error_type:
    ref->ATTR(type) = BaseType::Error;
    ref->ATTR(sym) = NULL;
    return;
}

/* Visits an ast::IndexExpr node.
 *
 * PARAMETERS:
 *   e     - the ast::IndexExpr node
 */
void SemPass2::visit(ast::IndexExpr *e) {
    e->expr->accept(this);
    if (e->index != NULL)
        e->index->accept(this);
}

/* Visits an ast::VarDecl node.
 *
 * PARAMETERS:
 *   decl     - the ast::VarDecl node
 */
void SemPass2::visit(ast::VarDecl *decl) {
    if (decl->init) {
        decl->init->accept(this);
        if (!decl->type->ATTR(type)->compatible(decl->init->ATTR(type))) {
            issue(decl->getLocation(), 
                    new IncompatibleError(decl->init->ATTR(type), decl->type->ATTR(type)));
        }
    }

}

/* Visits an ast::AssignExpr node.
 *
 * PARAMETERS:
 *   e     - the ast::AssignExpr node
 */
void SemPass2::visit(ast::AssignExpr *s) {
    s->left->accept(this);
    s->e->accept(this);

    if (!isErrorType(s->left->ATTR(type)) &&
        !s->e->ATTR(type)->compatible(s->left->ATTR(type))) {
        issue(s->getLocation(),
              new IncompatibleError(s->left->ATTR(type), s->e->ATTR(type)));
    }

    s->ATTR(type) = s->left->ATTR(type);
}

/* Step6 begin */
/* Visits an ast::IfExpr node.
 *
 * PARAMETERS:
 *   e     - the ast::IfExpr node
 */
void SemPass2::visit(ast::IfExpr *s) {
    s->condition->accept(this);
    if (!s->condition->ATTR(type)->equal(BaseType::Int)) {
        issue(s->condition->getLocation(), new BadTestExprError());
        ;
    }

    s->true_brch->accept(this);
    s->false_brch->accept(this);

    s->ATTR(type) = BaseType::Int;
}
/* Step6 end */

/* Visits an ast::ExprStmt node.
 *
 * PARAMETERS:
 *   e     - the ast::ExprStmt node
 */
void SemPass2::visit(ast::ExprStmt *s) { s->e->accept(this); }

/* Visits an ast::IfStmt node.
 *
 * PARAMETERS:
 *   e     - the ast::IfStmt node
 */
void SemPass2::visit(ast::IfStmt *s) {
    s->condition->accept(this);
    if (!s->condition->ATTR(type)->equal(BaseType::Int)) {
        issue(s->condition->getLocation(), new BadTestExprError());
        ;
    }

    s->true_brch->accept(this);
    s->false_brch->accept(this);
}

/* Visits an ast::CompStmt node.
 *
 * PARAMETERS:
 *   c     - the ast::CompStmt node
 */
void SemPass2::visit(ast::CompStmt *c) {
    scopes->open(c->ATTR(scope));
    for (auto it = c->stmts->begin(); it != c->stmts->end(); ++it)
        (*it)->accept(this);
    scopes->close();
}
/* Visits an ast::WhileStmt node.
 *
 * PARAMETERS:
 *   e     - the ast::WhileStmt node
 */
void SemPass2::visit(ast::WhileStmt *s) {
    s->condition->accept(this);
    if (!s->condition->ATTR(type)->equal(BaseType::Int)) {
        issue(s->condition->getLocation(), new BadTestExprError());
    }

    s->loop_body->accept(this);
}

/* Step8 begin */

/* Visits an ast::ForStmt node.
 *
 * PARAMETERS:
 *   s     - the ast::ForStmt node
 */
void SemPass2::visit(ast::ForStmt *s) {
    scopes->open(s->ATTR(scope));

    if (s->cond) {
        s->cond->accept(this);
        if (!s->cond->ATTR(type)->equal(BaseType::Int)) {
            issue(s->cond->getLocation(), new BadTestExprError());
        }
    }

    if (s->init) s->init->accept(this);
    if (s->iter) s->iter->accept(this);
    s->body->accept(this);
    
    scopes->close();
}

/* Visits an ast::DoWhileStmt node.
 *
 * PARAMETERS:
 *   s     - the ast::DoWhileStmt node
 */
void SemPass2::visit(ast::DoWhileStmt *s) {
    s->loop_body->accept(this);
    s->condition->accept(this);
    if (!s->condition->ATTR(type)->equal(BaseType::Int)) {
        issue(s->condition->getLocation(), new BadTestExprError());
    }
}


/* Step8 end*/

/* Step9 begin */

/* Visits an ast::CallExpr node.
 *
 * PARAMETERS:
 *   e     - the ast::CallExpr node
 */

void SemPass2::visit(ast::CallExpr *e) {
    Symbol *s = scopes->lookup(e->ident, e->getLocation());
    Symbol *sym = scopes->lookup(e->ident + "__##DEF##__", e->getLocation());

    if (NULL == s && NULL == sym) {
        issue(e->getLocation(), new SymbolNotFoundError(e->ident));
    } else {
        Function *f = NULL;
        if (NULL != s) { // Function have implementation
            if (!s->isFunction())
                issue(e->getLocation(), new NotMethodError(s));
            else
                f = (Function *)s;
        } else { // Function have only declarations
            if (!sym->isFunction())
                issue(e->getLocation(), new NotMethodError(sym));
            else 
                f = (Function *)sym;
        }

        e->ATTR(type) = f->getResultType();
        e->ATTR(sym) = f;
            
        ast::ExprList::iterator it;
        util::List<Type *> *type_list = f->getType()->getArgList();
        util::List<Type *>::iterator type_it = type_list->begin();

        // check length
        if (type_list->length() != e->args->length()) {
            issue(e->getLocation(), new BadArgCountError(f));
        }

        // compare arg type
        for (it = e->args->begin(); it != e->args->end(); ++it){
            (*it)->accept(this);
            if (!(*it)->ATTR(type)->equal(*type_it))
                issue(e->getLocation(), new UnexpectedTypeError((*it)->ATTR(type), *type_it));
            type_it++;
        }
    }

    return;
}
/* Step9 end */

/* Visits an ast::ReturnStmt node.
 *
 * PARAMETERS:
 *   e     - the ast::ReturnStmt node
 */
void SemPass2::visit(ast::ReturnStmt *s) {
    s->e->accept(this);
    if (!isErrorType(retType) && !s->e->ATTR(type)->compatible(retType)) {
        issue(s->e->getLocation(),
              new IncompatibleError(retType, s->e->ATTR(type)));
    }
}

/* Visits an ast::FunDefn node.
 *
 * PARAMETERS:
 *   e     - the ast::FunDefn node
 */
void SemPass2::visit(ast::FuncDefn *f) {
    // Check Symbol
    Symbol *s = scopes->lookup(f->name, f->getLocation());
    Symbol *sdef = scopes->lookup(f->name + "__##DEF##__", f->getLocation());

    if (NULL == s && NULL == sdef) {
        issue(f->getLocation(), new SymbolNotFoundError(f->name));
    } else {
        Function *func;
        if (s != NULL) {
            if (!s->isFunction())
                issue(f->getLocation(), new NotMethodError(s));
            else
                func = (Function *)s;
        } else {
            if (!sdef->isFunction())
                issue(f->getLocation(), new NotMethodError(sdef));
            else
                func = (Function *)sdef;
        }

        if (f->ret_type->ATTR(type)->compatible(func->getResultType()) == false)
            issue(f->getLocation(), new IncompatibleError(func->getResultType(), f->ret_type->ATTR(type)));

        retType = f->ret_type->ATTR(type);

        scopes->open(f->ATTR(sym)->getAssociatedScope());
        for (auto it = f->stmts->begin(); it != f->stmts->end(); ++it)
            (*it)->accept(this);
        scopes->close();

        // compare type about declaration and implementation
        if (s != NULL && sdef != NULL) {
            util::List<Type *> *org_tlist = ((Function *)s)->getType()->getArgList();
            util::List<Type *> *def_tlist = ((Function *)sdef)->getType()->getArgList();
            util::List<Type *>::iterator org_it = org_tlist->begin();
            util::List<Type *>::iterator def_it = def_tlist->begin();

            // check length
            if (org_tlist->length() != def_tlist->length()) {
                issue(f->getLocation(), new DeclConflictError(f->name, s));
            }

            // check arg
            for (; org_it != org_tlist->end(); ++org_it, ++def_it) {
                if ((*org_it)->equal(*def_it) == false) {
                    issue(f->getLocation(), new DeclConflictError(f->name, s));
                }
            }
        }
    }
}

/* Visits an ast::Program node.
 *
 * PARAMETERS:
 *   e     - the ast::Program node
 */
void SemPass2::visit(ast::Program *p) {
    scopes->open(p->ATTR(gscope));
    for (auto it = p->func_and_globals->begin();
         it != p->func_and_globals->end(); ++it)
        (*it)->accept(this);
    scopes->close(); // close the global scope
}

/* Checks the types of all the expressions.
 *
 * PARAMETERS:
 *   tree  - AST of the program
 */
void MindCompiler::checkTypes(ast::Program *tree) {
    tree->accept(new SemPass2());
}


