/**
 * Recreate the qmake project parser from the grammar.
 */

import pegged.grammar;
import qmakegrammar;

void main()
{
    asModule!()("qmakeparser", "source/qmakeparser", QMakeGrammar);
}

