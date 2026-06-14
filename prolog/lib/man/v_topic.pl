/*  Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker and Anjo Anjewierden
    E-mail:        jan@swi.psy.uva.nl
    WWW:           http://www.swi.psy.uva.nl/projects/xpce/
    Copyright (c)  1985-2002, University of Amsterdam
    All rights reserved.

    Redistribution and use in source and binary forms, with or without
    modification, are permitted provided that the following conditions
    are met:

    1. Redistributions of source code must retain the above copyright
       notice, this list of conditions and the following disclaimer.

    2. Redistributions in binary form must reproduce the above copyright
       notice, this list of conditions and the following disclaimer in
       the documentation and/or other materials provided with the
       distribution.

    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
    "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
    LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
    FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
    COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
    INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
    BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
    LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
    CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
    LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
    ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
    POSSIBILITY OF SUCH DAMAGE.
*/

:- module(man_topic, []).

:- use_module(library(pce)).
:- require([ get_chain/3
           , member/2
           , send_list/3
           ]).

:- pce_begin_class(man_topic_browser, man_frame,
                   "Topic browser").

variable(topics,        man_module,             get, "`Topic' manual module").
variable(selection,     man_topic_card*,        get, "Selected topic").

initialise(TB, Manual:man_manual) :->
    "Create from Manual"::
    send(TB, send_super, initialise, Manual, 'Topic Browser'),

    get(Manual, module, topics, @on, Module),
    send(TB, slot, topics, Module),

    picture(Picture, Module),
    dialog(Dialog),

    send(TB, append, Picture),
    send(Dialog, below, Picture),
    send(TB, expand_node, TB?tree?root),

    send(TB, open).


                /********************************
                *            DIALOG             *
                ********************************/

dialog(D) :-
    new(D, dialog),
    new(TB, D?frame),

    send(D, append, button(help,   message(TB, help))),
    send(D, append, button(quit,   message(TB, quit))).



                /********************************
                *          COMMUNICATION        *
                ********************************/



node(TB, Card:man_topic_card, Node) :<-
    "Find node displaying card"::
    get(TB?tree, find, @arg1?card == Card, Node).


selected(TB, Obj:object*) :->
    "Set the selection"::
    get(TB, tree, Tree),
    send(Tree, for_all, message(@arg1, inverted, @off)),
    (   Obj \== @nil,
        send(Obj, instance_of, man_topic_card)
    ->  send(TB, slot, selection, Obj),
        (   get(TB, node, Obj, Node)
        ->  send(Node, inverted, @on)
        ;   true
        )
    ;   send(TB, slot, selection, @nil)
    ).


release_selection(TB) :->
    send(TB, selected, @nil).


unrelated(TB, From:object*, Rel:name, To:object*) :->
    "Trap delated relations"::
    send(TB, related, From, Rel, To).


related(TB, From:object*, Rel:name, _To:object*) :->
    "Trap added relations"::
    Rel == see_also,
    get(TB, current, From),
    send(TB, update_related).


                /********************************
                *            PICTURE            *
                ********************************/

:- pce_global(@man_topic_node_handler, make_man_topic_node_handler).

make_man_topic_node_handler(H) :-
    new(TB, @arg1?frame),
    new(Manual, TB?manual),
    new(Selection, Manual?selection),
    Node = @arg1,
    new(Card, Node?card),

    new(P, popup),
    send_list(P, append,
              [ menu_item(select,
                          message(TB, request_selection, Card, @on),
                          @default, @on,
                          Selection \== Card)
              , menu_item(expand,
                          message(TB, expand_node, Node),
                          @default, @off,
                          and(message(Node?sons, empty),
                              ?(Card, man_related, subs)))
              , menu_item(expand_tree,
                          message(TB, expand_tree, Node),
                          @default, @off,
                          ?(Card, man_related, subs))
              , menu_item(collapse_node,
                          message(TB, collapse_node, Node),
                          @default, @on,
                          not(message(Node?sons, empty)))
              ]),

    HNode = @receiver,
    new(HTool, HNode?frame),

    new(H, handler_group(popup_gesture(P),
                         click_gesture(left, '', single,
                                       message(HTool, request_selection,
                                               HNode?card, @off)),
                         click_gesture(left, '', double,
                                       message(HTool, request_selection,
                                               HNode?card, @on)))).


picture(P, Topics) :-
    new(P, picture),
    (   get(Topics, card, root, Root)
    ->  true
    ;   new(Root, man_topic_card(Topics, 'Contents', root)),
        send(Root, store, summary, string("Root of the topic index"))
    ),
    create_node(Root, Node),
    send(P, display, new(T, tree(Node))),
    send(T, level_gap, 25),
    send(T, node_handler, @man_topic_node_handler).


create_node(Card, Node) :-
    (   get(Card, related, subs, Subs),
        Subs \== @nil,
        \+ send(Subs, empty)
    ->  Font = bold
    ;   Font = normal
    ),
    new(Node, node(text(Card?name, left, Font))),
    send(Node, attribute, attribute(card, Card)).


tree(TB, Tree) :<-
    "Get the associated tree object"::
    get(TB?picture_member, tree_member, Tree).


expand_node(_TB, Node:node) :->
    "Make all sub-topics visible"::
    (   get(Node?card, related, subs, SubCards)
    ->  send(SubCards, for_all, message(@prolog, add_card, Node, @arg1))
    ;   true
    ).

add_card(Node, Card) :-
    (   get(Node?sons, find, @arg1?card == Card, _)
    ->  true
    ;   create_node(Card, Sub),
        send(Node, son, Sub)
    ).


expand_tree(TB, Node:node) :->
    "Expand all nodes below this one"::
    send(TB, expand_node, Node),
    send(Node?sons, for_all, message(TB, expand_tree, @arg1)).


collapse_node(_TB, Node:node) :->
    "Undisplay all subnodes"::
    send(Node?sons, for_all, message(@arg1, delete_tree)).


:- pce_end_class.
