/*****************************************************
 *  Implementation of "VarRef".
 *
 *  Please refer to ast/ast.hpp for the definition.
 *
 *  Keltin Leung 
 *
 *  removed owner
 */

#include "ast/ast.hpp"
#include "ast/visitor.hpp"
#include "config.hpp"

using namespace mind;
using namespace mind::ast;

/* Creates a new ArrayRef node.
 *
 * PARAMETERS:
 *   n       - name of the referenced variable
 *   id      - index expr
 *   l       - position in the source text
 */
ArrayRef::ArrayRef(std::string n, IndexExpr *id, Location *l) {

    setBasicInfo(ARRAY_REF, l);

    var = n;
    index = id;

    ATTR(sym) = NULL;
}

/* Visits the current node.
 *
 * PARAMETERS:
 *   v       - the visitor
 */
void ArrayRef::accept(Visitor *v) { v->visit(this); }

/* Prints the current AST node.
 *
 * PARAMETERS:
 *   os      - the output stream
 */
void ArrayRef::dumpTo(std::ostream &os) {
    ASTNode::dumpTo(os);
    os << " " << '"' << var << '"';
    newLine(os);
    if (index != NULL)
        os << index;
    decIndent(os);
}
