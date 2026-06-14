# class chain_hyper {#class-chain_hyper}

Class chain_hyper is a subclass of the general binary relation
class hyper.  It redefines the ->unlink_from and ->unlink_to
methods, making both objects dependent.  I.e. if the <-from
object is deleted, the ->to will be sent a ->destroy or ->free
and visa-versa.

If dependent destruction is not desired after all, the user
should first ->free the hyper-relation.


## Send methods {#class-chain_hyper-send}

- chain_hyper->unlink_to
    ->free <-from.

- chain_hyper->unlink_from
    If the <-to objects is alreeady being destroyed (see
    ->unlinking), do nothing.  Else, if the <-to object understands
    ->destroy, invoke this method, else invoke ->free
