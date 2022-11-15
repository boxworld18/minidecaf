/*****************************************************
 *  Implementation of "IntType".
 *
 *  Please refer to ast/ast.hpp for the definition.
 *
 *  Keltin Leung 
 */

#include "ast/ast.hpp"
#include "ast/visitor.hpp"
#include "config.hpp"

using namespace mind;
using namespace mind::ast;

/* Creates a new ArrayType node.
 *
 * PARAMETERS:
 *   i       - index expression
 *   l       - position in the source text
 */
ArrayType::ArrayType(DimList *i, Location *l) { 
    setBasicInfo(ARRAY_TYPE, l);

    index = i;
}

/* Visits the current node.
 *
 * PARAMETERS:
 *   v       - the visitor
 */
void ArrayType::accept(Visitor *v) { v->visit(this); }

/* Prints the current AST node.
 *
 * PARAMETERS:
 *   os      - the output stream
 */
void ArrayType::dumpTo(std::ostream &os) {
    ASTNode::dumpTo(os);
    if (index != NULL) {
        for (auto it = index->rbegin(); it != index->rend(); ++it)
            os << "[" << *it << "]";
    }
    decIndent(os);
}
