# class label_box {#class-label_box}

Class label_box provides the infrastructure to easily defined
new dialog_item objects from components.   It provides the
functionality to deal with the item's <->label and actions
by providing skeletons for ->apply <->message and <->modified.


## Send methods {#class-label_box-send}

- label_box->initialise: name=[name], message=[code]*
    Create an empty label_box object with the given name and
    associated action.

