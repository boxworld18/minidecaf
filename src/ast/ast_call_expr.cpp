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

/* Creates a new CallExpr node.
 *
 * PARAMETERS:
 *   n       - name of the function
 *   elist   - list of the expressions in the function call
 *   l       - position in the source text
 */
CallExpr::CallExpr(std::string n, ExprList *elist, Location *l) {

    setBasicInfo(CALL_EXPR, l);

    ident = n;
    args = elist;
}

/* Visits the current node.
 *
 * PARAMETERS:
 *   v       - the visitor
 */
void CallExpr::accept(Visitor *v) { v->visit(this); }

/* Prints the current AST node.
 *
 * PARAMETERS:
 *   os      - the output stream
 */
void CallExpr::dumpTo(std::ostream &os) {
    ASTNode::dumpTo(os);
    newLine(os);
    os << '"' << ident << '"';
    newLine(os);
    os << args << ")";
    decIndent(os);
}

