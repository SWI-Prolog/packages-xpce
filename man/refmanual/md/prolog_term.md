# class prolog_term {#class-prolog_term}

Class prolog_term is created by the XPCE/Prolog interface as a
subclass of class host_data.  It represents arbitrary Prolog
terms, either as a handle to an term on the Prolog stack (term_t
in SWI-Prolog, QP_term_ref in Quintus), or a reference to a
term recorded in the Prolog database.

Instances of prolog_term are created using a direct reference to
the prolog term.  If the interface can no longer guarantee the
lifefime of the term-reference, the interface checks whether the
prolog_term object has references.  If so, the term is recorded
into the Prolog database, and the handle of the prolog_term
object is changed to be a reference to the recorded term.  If
there are no references, the prolog_term object is simply
discarded.

The XPCE type `prolog` is a disjunction of type prolog_term and
type atomic (implying name, integer and real).


## Send methods {#class-prolog_term-send}

- prolog_term->unlink
    If the prolog_term object refers to a term recorded into the
    Prolog database, ->unlink will remove it.  This effectively
    leaves garbage collection of terms associated with XPCE
    data to XPCE.


## Get methods {#class-prolog_term-get}

- prolog_term<-print_name: -> string
    Returns a string holding the term represented as text produced
    by write/1.

