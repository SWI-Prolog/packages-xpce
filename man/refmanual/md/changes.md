# Changes {#sec-changes}

## menu->condition {#sec-changes-menu-condition}

Changed binding of @receiver

When a `menu_item <-condition` is executed by `menu->update`, it now
passed the menu_item rather than the menu as a whole for @receiver.

## text_item ->typed {#sec-changes-text_item-typed}

Only accepts event_id

This method is now defined to accept an event-id.  `text_item ->event`
invokes `text_item ->typed: event`, which is converted by the
type-checker.

