/*****************************************************
 *  Implementation of the third translation pass.
 *
 *  In the third pass, we will:
 *    translate all the statements and expressions
 *
 *  Keltin Leung 
 */

#include "translation.hpp"
#include "asm/offset_counter.hpp"
#include "ast/ast.hpp"
#include "compiler.hpp"
#include "config.hpp"
#include "scope/scope.hpp"
#include "symb/symbol.hpp"
#include "tac/tac.hpp"
#include "tac/trans_helper.hpp"
#include "type/type.hpp"

using namespace mind;
using namespace mind::symb;
using namespace mind::tac;
using namespace mind::type;
using namespace mind::assembly;

/* Constructor.
 *
 * PARAMETERS:
 *   helper - the translation helper
 */
Translation::Translation(tac::TransHelper *helper) {
    mind_assert(NULL != helper);

    tr = helper;
}

/* Translating an ast::Program node.
 */
void Translation::visit(ast::Program *p) {
    for (auto it = p->func_and_globals->begin();
         it != p->func_and_globals->end(); ++it)
        (*it)->accept(this);
}

// three sugars for parameter offset management
#define RESET_OFFSET() tr->getOffsetCounter()->reset(OffsetCounter::PARAMETER)
#define NEXT_OFFSET(x) tr->getOffsetCounter()->next(OffsetCounter::PARAMETER, x)

/* Translating an ast::FuncDefn node.
 *
 * NOTE:
 *   call tr->startFunc() before translating the statements and
 *   call tr->endFunc() after all the statements have been translated
 */
void Translation::visit(ast::FuncDefn *f) {
    Function *fun = f->ATTR(sym);
    assert(fun != NULL);

    // attaching function entry label
    if (NULL == fun->getEntryLabel())
        fun->attachEntryLabel(tr->getNewEntryLabel(fun));

    if (f->forward_decl) return;

    // arguments
    int order = 0;
    for (auto it = f->formals->begin(); it != f->formals->end(); ++it) {
        auto v = (*it)->ATTR(sym);
        assert(v != NULL);
        v->setOrder(order++);
        v->attachTemp(tr->getNewTempI4());
    }
    fun->offset = fun->getOrder() * POINTER_SIZE;
    
    RESET_OFFSET();
    
    // You may process params here, i.e use reg or stack to pass parameters
    int cnt = 0;
    for (auto it = f->formals->begin(); it != f->formals->end(); ++it) {
        auto v = (*it)->ATTR(sym);
        if (cnt >= 8) // use stack
            v->offset = NEXT_OFFSET(v->getTemp()->size);
        cnt++;
    }
    
    tr->startFunc(fun);

    cnt = 0;
    for (auto it = f->formals->begin(); it != f->formals->end(); ++it) {
        auto v = (*it)->ATTR(sym);
        tr->genPop(v->getTemp());
        if (++cnt >= 8) break;
    }
    
    // translates statement by statement
    for (auto it = f->stmts->begin(); it != f->stmts->end(); ++it) 
        (*it)->accept(this);
    
    tr->genReturn(tr->genLoadImm4(0)); // Return 0 by default
    
    tr->endFunc();
}

/* Step9 begin */

/* Translating an ast::CallExpr node.
 *
 */
void Translation::visit(ast::CallExpr *s) {
    Function *fun = s->ATTR(sym);
    assert(fun != NULL);

    // accept parameters
    for (auto it = s->args->begin(); it != s->args->end(); ++it) 
        (*it)->accept(this);
    
    // push parameters
    for (auto it = s->args->begin(); it != s->args->end(); ++it)
        tr->genPush((*it)->ATTR(val));
    
    // call function
    Temp t = tr->genCall(fun->getEntryLabel());
    s->ATTR(val) = t;
}

/* Step9 end */

/* Translating an ast::AssignStmt node.
 *
 * NOTE:
 *   different kinds of Lvalue require different translation
 */
void Translation::visit(ast::AssignExpr *s) {
    s->left->accept(this);
    s->e->accept(this);

    switch (s->left->ATTR(lv_kind)) {
        case ast::Lvalue::SIMPLE_VAR: {
            const auto &sym = ((ast::VarRef*)s->left)->ATTR(sym);
            if (sym->isGlobalVar()) {
                // Global variables
                Temp t = tr->genLoadSymbol(sym->getName());
                tr->genStore(s->e->ATTR(val), t, 0);
                s->ATTR(val) = s->e->ATTR(val);
            } else {
                // Local variables
                s->ATTR(val) = sym->getTemp();
                tr->genAssign(s->ATTR(val), s->e->ATTR(val));
            }
            break;
        }
        case ast::Lvalue::ARRAY_ELE: {
            // TODO: Array
            break;
        }
        default:
            mind_assert(false); // impossible
    }

    tr->genAssign(s->ATTR(val), s->e->ATTR(val));
}

/* Translating an ast::ExprStmt node.
 */
void Translation::visit(ast::ExprStmt *s) { s->e->accept(this); }

/* Translating an ast::IfStmt node.
 *
 * NOTE:
 *   you don't need to test whether the false_brch is empty
 */
void Translation::visit(ast::IfStmt *s) {
    Label L1 = tr->getNewLabel(); // entry of the false branch
    Label L2 = tr->getNewLabel(); // exit
    s->condition->accept(this);
    tr->genJumpOnZero(L1, s->condition->ATTR(val));

    s->true_brch->accept(this);
    tr->genJump(L2); // done

    tr->genMarkLabel(L1);
    s->false_brch->accept(this);

    tr->genMarkLabel(L2);
}

/* Step6 begin */
/* Translating an ast::IfExpr node.
 *
 */
void Translation::visit(ast::IfExpr *s) {
    Label L1 = tr->getNewLabel(); // entry of the false branch
    Label L2 = tr->getNewLabel(); // exit

    Temp t = tr->getNewTempI4();

    s->condition->accept(this);
    tr->genJumpOnZero(L1, s->condition->ATTR(val));

    s->true_brch->accept(this);
    tr->genAssign(t, s->true_brch->ATTR(val));
    tr->genJump(L2);

    tr->genMarkLabel(L1);
    s->false_brch->accept(this);
    tr->genAssign(t, s->false_brch->ATTR(val));
    
    tr->genMarkLabel(L2);
    s->ATTR(val) = t;
}
/* Step6 end */

/* Translating an ast::WhileStmt node.
 */
void Translation::visit(ast::WhileStmt *s) {
    Label L1 = tr->getNewLabel();
    Label L2 = tr->getNewLabel();

    Label old_break = current_break_label;
    Label old_continue = current_continue_label;

    current_continue_label = L1;
    current_break_label = L2;

    tr->genMarkLabel(L1);
    s->condition->accept(this);
    tr->genJumpOnZero(L2, s->condition->ATTR(val));

    s->loop_body->accept(this);
    tr->genJump(L1);

    tr->genMarkLabel(L2);

    current_continue_label = old_continue;
    current_break_label = old_break;
}

/* Translating an ast::BreakStmt node.
 */
void Translation::visit(ast::BreakStmt *s) { tr->genJump(current_break_label); }

/* Step8 begin */

/* Translating an ast::DoWhileStmt node.
 */
void Translation::visit(ast::DoWhileStmt *s) {
    Label L1 = tr->getNewLabel();
    Label L2 = tr->getNewLabel();

    Label old_break = current_break_label;
    Label old_continue = current_continue_label;

    current_continue_label = L1;
    current_break_label = L2;

    tr->genMarkLabel(L1);
    s->loop_body->accept(this);

    s->condition->accept(this);
    tr->genJumpOnZero(L2, s->condition->ATTR(val));
    tr->genJump(L1);

    tr->genMarkLabel(L2);

    current_continue_label = old_continue;
    current_break_label = old_break;
}

/* Translating an ast::ForStmt node.
 */
void Translation::visit(ast::ForStmt *s) {
    Label L1 = tr->getNewLabel();
    Label L2 = tr->getNewLabel();
    Label L3 = tr->getNewLabel();

    Label old_break = current_break_label;
    Label old_continue = current_continue_label;

    current_continue_label = L2;
    current_break_label = L3;

    if (s->init) s->init->accept(this);
    tr->genMarkLabel(L1);

    if (s->cond) {
        s->cond->accept(this);
        tr->genJumpOnZero(L3, s->cond->ATTR(val));
    }

    s->body->accept(this);

    tr->genMarkLabel(L2);

    if (s->iter) s->iter->accept(this);
    tr->genJump(L1);

    tr->genMarkLabel(L3);

    current_continue_label = old_continue;
    current_break_label = old_break;
}

/* Translating an ast::ContStmt node.
 */
void Translation::visit(ast::ContStmt *s) { tr->genJump(current_continue_label); }

/* Step8 end */

/* Translating an ast::CompStmt node.
 */
void Translation::visit(ast::CompStmt *c) {
    // translates statement by statement
    for (auto it = c->stmts->begin(); it != c->stmts->end(); ++it)
        (*it)->accept(this);
}
/* Translating an ast::ReturnStmt node.
 */
void Translation::visit(ast::ReturnStmt *s) {
    s->e->accept(this);
    tr->genReturn(s->e->ATTR(val));
}

/* Translating an ast::AddExpr node.
 */
void Translation::visit(ast::AddExpr *e) {
    e->e1->accept(this);
    e->e2->accept(this);

    e->ATTR(val) = tr->genAdd(e->e1->ATTR(val), e->e2->ATTR(val));
}

/* Step3 started */

/* Translating an ast::SubExpr node.
 */
void Translation::visit(ast::SubExpr *e) {
    e->e1->accept(this);
    e->e2->accept(this);

    e->ATTR(val) = tr->genSub(e->e1->ATTR(val), e->e2->ATTR(val));
}

/* Translating an ast::MulExpr node.
 */
void Translation::visit(ast::MulExpr *e) {
    e->e1->accept(this);
    e->e2->accept(this);

    e->ATTR(val) = tr->genMul(e->e1->ATTR(val), e->e2->ATTR(val));
}

/* Translating an ast::DivExpr node.
 */
void Translation::visit(ast::DivExpr *e) {
    e->e1->accept(this);
    e->e2->accept(this);

    e->ATTR(val) = tr->genDiv(e->e1->ATTR(val), e->e2->ATTR(val));
}

/* Translating an ast::ModExpr node.
 */
void Translation::visit(ast::ModExpr *e) {
    e->e1->accept(this);
    e->e2->accept(this);

    e->ATTR(val) = tr->genMod(e->e1->ATTR(val), e->e2->ATTR(val));
}

/* Step3 ended */

/* Step4 started */

/* Translating an ast::EquExpr node.
 */
void Translation::visit(ast::EquExpr *e) {
    e->e1->accept(this);
    e->e2->accept(this);

    e->ATTR(val) = tr->genEqu(e->e1->ATTR(val), e->e2->ATTR(val));
}

/* Translating an ast::NeqExpr node.
 */
void Translation::visit(ast::NeqExpr *e) {
    e->e1->accept(this);
    e->e2->accept(this);

    e->ATTR(val) = tr->genNeq(e->e1->ATTR(val), e->e2->ATTR(val));
}

/* Translating an ast::AndExpr node.
 */
void Translation::visit(ast::AndExpr *e) {
    e->e1->accept(this);
    e->e2->accept(this);

    e->ATTR(val) = tr->genLAnd(e->e1->ATTR(val), e->e2->ATTR(val));
}

/* Translating an ast::OrExpr node.
 */
void Translation::visit(ast::OrExpr *e) {
    e->e1->accept(this);
    e->e2->accept(this);

    e->ATTR(val) = tr->genLOr(e->e1->ATTR(val), e->e2->ATTR(val));
}

/* Translating an ast::GeqExpr node.
 */
void Translation::visit(ast::GeqExpr *e) {
    e->e1->accept(this);
    e->e2->accept(this);

    e->ATTR(val) = tr->genGeq(e->e1->ATTR(val), e->e2->ATTR(val));
}

/* Translating an ast::LeqExpr node.
 */
void Translation::visit(ast::LeqExpr *e) {
    e->e1->accept(this);
    e->e2->accept(this);

    e->ATTR(val) = tr->genLeq(e->e1->ATTR(val), e->e2->ATTR(val));
}

/* Translating an ast::LesExpr node.
 */
void Translation::visit(ast::LesExpr *e) {
    e->e1->accept(this);
    e->e2->accept(this);

    e->ATTR(val) = tr->genLes(e->e1->ATTR(val), e->e2->ATTR(val));
}

/* Translating an ast::GrtExpr node.
 */
void Translation::visit(ast::GrtExpr *e) {
    e->e1->accept(this);
    e->e2->accept(this);

    e->ATTR(val) = tr->genGtr(e->e1->ATTR(val), e->e2->ATTR(val));
}

/* Step4 ended */

/* Translating an ast::IntConst node.
 */
void Translation::visit(ast::IntConst *e) {
    e->ATTR(val) = tr->genLoadImm4(e->value);
}

/* Translating an ast::NegExpr node.
 */
void Translation::visit(ast::NegExpr *e) {
    e->e->accept(this);

    e->ATTR(val) = tr->genNeg(e->e->ATTR(val));
}

/* Step2 started */

/* Translating an ast::NotExpr node.
 */
void Translation::visit(ast::NotExpr *e) {
    e->e->accept(this);

    e->ATTR(val) = tr->genLNot(e->e->ATTR(val));
}

/* Translating an ast::BitNotExpr node.
 */
void Translation::visit(ast::BitNotExpr *e) {
    e->e->accept(this);

    e->ATTR(val) = tr->genBNot(e->e->ATTR(val));
}

/* Step2 ended */

/* Translating an ast::LvalueExpr node.
 *
 * NOTE:
 *   different Lvalue kinds need different translation
 */
void Translation::visit(ast::LvalueExpr *e) {
    e->lvalue->accept(this);

    switch (e->lvalue->ATTR(lv_kind)) {
        case ast::Lvalue::SIMPLE_VAR: {
            const auto &sym = ((ast::VarRef*)e->lvalue)->ATTR(sym);
            if (sym->isGlobalVar()) {
                // Global variables
                Temp t = tr->genLoadSymbol(sym->getName());
                e->ATTR(val) = tr->genLoad(t, 0);
            } else {
                // Local variables
                e->ATTR(val) = sym->getTemp();
            }
            break;
        }
        case ast::Lvalue::ARRAY_ELE: {
            // TODO: Array
            break;
        }
        default:
            mind_assert(false); // impossible
    }
}

/* Translating an ast::VarRef node.
 *
 * NOTE:
 *   there are two kinds of variable reference: member variables or simple
 * variables
 */
void Translation::visit(ast::VarRef *ref) {
    switch (ref->ATTR(lv_kind)) {
    case ast::Lvalue::SIMPLE_VAR:
        // nothing to do
        break;

    default:
        mind_assert(false); // impossible
    }
    // actually it is so simple :-)
}

/* Translating an ast::VarDecl node.
 */
void Translation::visit(ast::VarDecl *decl) {
    // TODO
    Variable *var = decl->ATTR(sym);
    Temp t = tr->getNewTempI4();
    var->attachTemp(t);

    if (decl->init != NULL) {
        decl->init->accept(this);
        tr->genAssign(t, decl->init->ATTR(val));
    }

}

/* Translates an entire AST into a Piece list.
 *
 * PARAMETERS:
 *   tree  - the AST
 * RETURNS:
 *   the result Piece list (represented by the first node)
 */
Piece *MindCompiler::translate(ast::Program *tree) {
    TransHelper *helper = new TransHelper(md);

    tree->accept(new Translation(helper));

    return helper->getPiece();
}
