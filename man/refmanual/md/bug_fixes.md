# Bug fixes {#sec-bug-fixes}

## image <-clip {#sec-bug_fixes-image-clip}

Fixed to actually use the area argument

## image <-postscript {#sec-bug_fixes-image-postscript}

Fixed for images loaded from file

## message passing {#sec-bug_fixes-message-passing}

Fail if object-defined method fails

When a message to an object activated an object-defined method and
this method failed, the class-defined method was still activated as
well.  This has been fixed.

