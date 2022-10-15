/*****************************************************
 *  Implementation of "ForStmt".
 *
 *  Please refer to ast/ast.hpp for the definition.
 *
 *  IL Iong
 */

#include "ast/ast.hpp"
#include "ast/visitor.hpp"
#include "config.hpp"

using namespace mind;
using namespace mind::ast;

/* Creates a new ForStmt node.
 *
 * PARAMETERS:
 *   cond    - the test expression
 *   body    - the loop body
 *   l       - position in the source text
 */
ForStmt::ForStmt(Statement *_init, Expr *_cond, Expr *_iter, Statement *_body, Location *l) {

    setBasicInfo(FOR_STMT, l);

    init = _init;
    cond = _cond;
    iter = _iter;
    body = _body;
}

/* Visits the current node.
 *
 * PARAMETERS:
 *   v       - the visitor
 */
void ForStmt::accept(Visitor *v) { v->visit(this); }

/* Prints the current AST node.
 *
 * PARAMETERS:
 *   os      - the output stream
 */
void ForStmt::dumpTo(std::ostream &os) {
    ASTNode::dumpTo(os);
    newLine(os);
    os << init;
    newLine(os);
    os << cond;
    newLine(os);
    os << iter;
    newLine(os);
    os << body << ")";
    decIndent(os);
}

/* Creates a new ContStmt node.
 *
 * PARAMETERS:
 *   l       - position in the source text
 */
ContStmt::ContStmt(Location *l) { setBasicInfo(CONT_STMT, l); }

/* Visits the current node.
 *
 * PARAMETERS:
 *   v       - the visitor
 */
void ContStmt::accept(Visitor *v) { v->visit(this); }

/* Prints the current AST node.
 *
 * PARAMETERS:
 *   os      - the output stream
 */
void ContStmt::dumpTo(std::ostream &os) {
    ASTNode::dumpTo(os);
    newLine(os);
    decIndent(os);
}

/* Creates a new NullExpr node.
 *
 * PARAMETERS:
 *   l       - position in the source text
 */
NullExpr::NullExpr(Location *l) { setBasicInfo(NULL_EXPR, l); }

/* Visits the current node.
 *
 * PARAMETERS:
 *   v       - the visitor
 */
void NullExpr::accept(Visitor *v) { v->visit(this); }

/* Prints the current AST node.
 *
 * PARAMETERS:
 *   os      - the output stream
 */
void NullExpr::dumpTo(std::ostream &os) {
    ASTNode::dumpTo(os);
    os << "Nullptr";
    newLine(os);
    decIndent(os);
}