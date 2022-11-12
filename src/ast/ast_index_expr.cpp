/*****************************************************
 *  Implementation of "CallExpr".
 *
 *  Please refer to ast/ast.hpp for the definition.
 *
 */

#include "ast/ast.hpp"
#include "ast/visitor.hpp"
#include "config.hpp"

using namespace mind;
using namespace mind::ast;

/* Creates a new IndexExpr node.
 *
 * PARAMETERS:
 *   e       - expr of this dimension
 *   i       - index of follow dimensions
 *   l       - position in the source text
 */
IndexExpr::IndexExpr(Expr *e, Location *l) {

    setBasicInfo(INDEX_EXPR, l);

    expr = e;
    index = NULL;
}

IndexExpr::IndexExpr(Expr *e, IndexExpr *i, Location *l) {

    setBasicInfo(INDEX_EXPR, l);

    expr = e;
    index = i;
}

/* Visits the current node.
 *
 * PARAMETERS:
 *   v       - the visitor
 */
void IndexExpr::accept(Visitor *v) { v->visit(this); }

/* Prints the current AST node.
 *
 * PARAMETERS:
 *   os      - the output stream
 */
void IndexExpr::dumpTo(std::ostream &os) {
    ASTNode::dumpTo(os);
    newLine(os);
    os << "([" << expr << "]";
    newLine(os);
    if (index != NULL)
        os << index << ")";
    decIndent(os);
}

