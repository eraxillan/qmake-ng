/**
 * Recreate the EP parser from the grammar.
 */

import pegged.grammar;
import qmakegrammar;

void main()
{
    // asModule!()("epparser", "source/epparser", EPgrammar, header);
    asModule!()("qmakeparser", "source/qmakeparser", QMakeGrammar);
}

