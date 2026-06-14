# class code_vector {#class-code_vector}

Argument vector that holds function objects without evaluating them.
A `code_vector` is a subclass of `vector` whose elements have type
`any|function`; storing a function does not invoke it.  Used by
the virtual machine for:

- the argument vectors of `message` and `?` (class obtain);
- packing variable-arity arguments behind a vararg type
  (`any ...`, etc.);
- passing arguments via `object ->send_vector` and the related
  hooks.

The only difference from `vector` is type-checking: `vector` declares
its elements of type `object`, forcing functions to evaluate on
assignment, while `code_vector` accepts unevaluated functions and
leaves them alone.

@see class vector
@see class message
@see class obtain
@see object->send_vector


## Send methods {#class-code_vector-send}

- code_vector->initialise: element=any|function ...
    Create a code_vector from the supplied elements (functions are
    stored as-is).

- code_vector->append: value=any|function ...
    Append one or more elements at the high end.

- code_vector->element: index=int, value=any|function
    Set the element at the indicated index without evaluating a
    function value.

- code_vector->fill: value=any|function, from=[int], to=[int]
    Fill the index range `[from, to]` with the same value (defaults
    to the whole vector).

- code_vector->unlink
    Clear the vector when its containing message is destroyed.


## Get methods {#class-code_vector-get}

- code_vector<-convert: any -> code_vector
    Convert `[]` (a Prolog empty list) into an empty code_vector.
    Used by the type-checker.
