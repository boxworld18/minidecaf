/*****************************************************
 *  Implementation of the first semantic analysis pass.
 *
 *  In the first pass, we will:
 *    1. create appropriate type::Type instances for the types;
 *    2. create and manage scope::Scope instances;
 *    3. create symb::Symbol instances;
 *    4. manage the symbol tables.
 *  After this pass, the ATTR(sym) or ATTR(type) attributs of the visited nodes
 *  should have been set.
 *
 *  Keltin Leung 
 */

#include "ast/ast.hpp"
#include "ast/visitor.hpp"
#include "compiler.hpp"
#include "config.hpp"
#include "scope/scope.hpp"
#include "scope/scope_stack.hpp"
#include "symb/symbol.hpp"
#include "type/type.hpp"

using namespace mind;
using namespace mind::scope;
using namespace mind::symb;
using namespace mind::type;
using namespace mind::err;

/* Pass 1 of the semantic analysis.
 */
class SemPass1 : public ast::Visitor {
  public:
    // visiting declarations
    virtual void visit(ast::FuncDefn *);
    virtual void visit(ast::Program *);
    virtual void visit(ast::IfStmt *);
    virtual void visit(ast::WhileStmt *);
    virtual void visit(ast::CompStmt *);
    virtual void visit(ast::VarDecl *);
    virtual void visit(ast::ForStmt *);
    virtual void visit(ast::DoWhileStmt *);
    // visiting types
    virtual void visit(ast::IntType *);
    virtual void visit(ast::ArrayType *);
};

/* Visiting an ast::Program node.
 *
 * PARAMETERS:
 *   prog  - the ast::Progarm node to visit
 */
void SemPass1::visit(ast::Program *prog) {
    prog->ATTR(gscope) = new GlobalScope();
    scopes->open(prog->ATTR(gscope));

    // visit global variables and each function
    for (auto it = prog->func_and_globals->begin();
         it != prog->func_and_globals->end(); ++it) {
        (*it)->accept(this);
        if ((*it)->getKind() == mind::ast::ASTNode::FUNC_DEFN &&
            std::string("main") ==
                dynamic_cast<mind::ast::FuncDefn *>(*it)->name)
            prog->ATTR(main) =
                dynamic_cast<mind::ast::FuncDefn *>(*it)->ATTR(sym);
    }

    scopes->close(); // close the global scope
}

/* Visiting an ast::FunDefn node.
 *
 * NOTE:
 *   tasks include:
 *   1. build up the Function symbol
 *   2. build up symbols of the parameters
 *   3. build up symbols of the local variables
 *
 *   we will check Declaration Conflict Errors for symbols declared in the SAME
 *   class scope, but we don't check such errors for symbols declared in
 *   different scopes here (we leave this task to checkOverride()).
 * PARAMETERS:
 *   fdef  - the ast::FunDefn node to visit
 */
void SemPass1::visit(ast::FuncDefn *fdef) {
    fdef->ret_type->accept(this);
    Type *t = fdef->ret_type->ATTR(type);

    if (fdef->forward_decl) {
        Function *f = new Function(fdef->name + "__##DEF##__", t, fdef->getLocation());
        Symbol *sym = scopes->lookup(fdef->name + "__##DEF##__", fdef->getLocation(), false);

        if (NULL != sym)
            f = (Function *) sym; 
        else 
            scopes->declare(f);

        fdef->ATTR(sym) = f;  

        // opens function scope
        FuncScope *my_scope = f->getAssociatedScope();
        assert(my_scope != NULL);

        // param list
        if (my_scope->_first) {
            // first time declare
            scopes->open(my_scope);

            // adds the parameters
            for (auto it = fdef->formals->begin(); it != fdef->formals->end(); ++it) {
                my_scope->_params.push_back((*it)->type);    
                (*it)->accept(this);
                f->appendParameter((*it)->ATTR(sym));
            }

            scopes->close();
            my_scope->_first = false;

        } else {
            // redeclare or implementation
            auto plist = my_scope->_params;
            
            // check length
            if (plist.size() != fdef->formals->length()) {
                issue(fdef->getLocation(), new DeclConflictError(fdef->name, sym));
                return;
            }

            // check each parameter
            auto it = fdef->formals->begin();
            for (size_t i = 0; i < plist.size(); i++) {
                if (plist[i]->getKind() != (*it)->type->getKind()) {
                    issue(fdef->getLocation(), new DeclConflictError(fdef->name, sym));
                    return;
                }
                it++;
            }
        }

    } else {
        Function *f = new Function(fdef->name, t, fdef->getLocation());
        fdef->ATTR(sym) = f;

        // checks the Declaration Conflict Error of Case 1 (but don't check Case
        // 2,3). if DeclConflictError occurs, we don't put the symbol into the
        // symbol table
        Symbol *sym = scopes->lookup(fdef->name, fdef->getLocation(), false);

        if (NULL != sym)
            issue(fdef->getLocation(), new DeclConflictError(fdef->name, sym));
        else
            scopes->declare(f);

        // Implementation
        scopes->open(f->getAssociatedScope());

        // adds the parameters
        for (auto it = fdef->formals->begin(); it != fdef->formals->end(); ++it) {
            (*it)->accept(this);
            f->appendParameter((*it)->ATTR(sym));
        }

        // adds the local variables
        for (auto it = fdef->stmts->begin(); it != fdef->stmts->end(); ++it)
            (*it)->accept(this);

        // closes function scope
        scopes->close();
    }
}

/* Visits an ast::IfStmt node.
 *
 * PARAMETERS:
 *   e     - the ast::IfStmt node
 */
void SemPass1::visit(ast::IfStmt *s) {
    s->condition->accept(this);
    s->true_brch->accept(this);
    s->false_brch->accept(this);
}

/* Visits an ast::WhileStmt node.
 *
 * PARAMETERS:
 *   e     - the ast::WhileStmt node
 */
void SemPass1::visit(ast::WhileStmt *s) {
    s->condition->accept(this);
    s->loop_body->accept(this);
}

/* Visits an ast::DoWhileStmt node.
 *
 * PARAMETERS:
 *   e     - the ast::DoWhileStmt node
 */
void SemPass1::visit(ast::DoWhileStmt *s) {
    s->loop_body->accept(this);
    s->condition->accept(this);
}

/* Visiting an ast::ForStmt node.
 */
void SemPass1::visit(ast::ForStmt *s) {
    // opens function scope
    Scope *scope = new LocalScope();
    s->ATTR(scope) = scope;
    scopes->open(scope);

    // adds the local variables
    if (s->init != NULL) s->init->accept(this);
    if (s->cond != NULL) s->cond->accept(this);
    if (s->iter != NULL) s->iter->accept(this);

    s->body->accept(this);

    // closes function scope
    scopes->close();
}


/* Visiting an ast::CompStmt node.
 */
void SemPass1::visit(ast::CompStmt *c) {
    // opens function scope
    Scope *scope = new LocalScope();
    c->ATTR(scope) = scope;
    scopes->open(scope);

    // adds the local variables
    for (auto it = c->stmts->begin(); it != c->stmts->end(); ++it)
        (*it)->accept(this);

    // closes function scope
    scopes->close();
}

/* Visiting an ast::VarDecl node.
 *
 * NOTE:
 *   we will check Declaration Conflict Errors for symbols declared in the SAME
 *   function scope, but we don't check such errors for symbols declared in
 *   different scopes here (we leave this task to checkOverride()).
 * PARAMETERS:
 *   vdecl - the ast::VarDecl node to visit
 */
void SemPass1::visit(ast::VarDecl *vdecl) {
    Type *t = NULL;

    vdecl->type->accept(this);
    t = vdecl->type->ATTR(type);

    // TODO: Add a new symbol to a scope
    // 1. Create a new `Variable` symbol
    Variable *v = new Variable(vdecl->name, t, vdecl->getLocation());

    // 2. Check for conflict in `scopes`, which is a global variable refering to
    // a scope stack
    Symbol *sym = scopes->lookup(vdecl->name, vdecl->getLocation(), false);
    
    // 3. Declare the symbol in `scopes`
    if (NULL != sym)
        issue(vdecl->getLocation(), new DeclConflictError(vdecl->name, sym));
    else
        scopes->declare(v);

    if (vdecl->init != NULL)
        vdecl->init->accept(this);

    // 4. Special processing for global variables
    if (v->isGlobalVar()) {
        if (v->getType()->isBaseType()) {
            if (vdecl->init != NULL) {
                if (vdecl->init->getKind() == ast::ASTNode::INT_CONST)
                    v->setGlobalInit(((ast::IntConst *)vdecl->init)->value);
                else
                    issue(vdecl->getLocation(), new SyntaxError("Global variable must be initialized with an integer constant"));
            }
        } else if (v->getType()->isArrayType()){
            if (vdecl->ainit != NULL)
                v->setArrayInit(vdecl->ainit);
        } else {
            issue(vdecl->getLocation(), new SyntaxError("Not supported type for global variable"));
        }
        
    }
    
    // 5. Tag the symbol to `vdecl->ATTR(sym)`
    vdecl->ATTR(sym) = v;
    
}

/* Visiting an ast::IntType node.
 *
 * PARAMETERS:
 *   itype - the ast::IntType node to visit
 */
void SemPass1::visit(ast::IntType *itype) { itype->ATTR(type) = BaseType::Int; }

/* Visiting an ast::ArrayType node.
 *
 * PARAMETERS:
 *   atype - the ast::ArrayType node to visit
 */
void SemPass1::visit(ast::ArrayType *atype) { 
    ast::DimList *iexpr = atype->index;
    assert(iexpr != NULL);
    Type *t = BaseType::Int;

    for (auto it = iexpr->begin(); it != iexpr->end(); ++it) {
        if (*it <= 0)
            issue(atype->getLocation(), new ZeroLengthedArrayError());
        t = new ArrayType(t, *it);
    }

    atype->ATTR(type) = t;
}

/* Builds the symbol tables for the Mind compiler.
 *
 * PARAMETERS:
 *   tree  - the AST of the program
 */
void MindCompiler::buildSymbols(ast::Program *tree) {
    tree->accept(new SemPass1());
}
