.. image:: https://github.com/countvajhula/mantra/actions/workflows/test.yml/badge.svg
    :target: https://github.com/countvajhula/mantra/actions

.. image:: https://coveralls.io/repos/github/countvajhula/mantra/badge.svg?branch=master
    :target: https://coveralls.io/github/countvajhula/mantra?branch=master

.. image:: https://melpa.org/packages/mantra-badge.svg
    :alt: MELPA
    :target: https://melpa.org/#/mantra

.. image:: https://stable.melpa.org/packages/mantra-badge.svg
    :alt: MELPA Stable
    :target: https://stable.melpa.org/#/mantra

mantra
===========
Parse and compose keyboard activity in Emacs.

This package could conceivably be used to implement packages resembling yasnippet, evil-repeat, Emacs's keyboard macro ring, Evil jumps and changes, winner mode, and more.

Installation
------------

Mantra is not on a package archive such as `MELPA <https://melpa.org/>`_ yet, but you can install it using `Straight.el <https://github.com/radian-software/straight.el>`_ (or `Elpaca <https://github.com/progfolio/elpaca>`_) by putting this somewhere in your :code:`.emacs.d`:

.. code-block:: elisp

  (use-package mantra
    :straight
    (mantra
      :type git
      :host github
      :repo "countvajhula/mantra"))

Use
---

Creating a Parser
~~~~~~~~~~~~~~~~~

To get started with Mantra, first create a parser.

A common input source we might want to parse is key sequences entered on the Emacs command loop. Let's parse this data to get a feel for things, and we'll start by writing the simplest possible parser: one that accepts any input given to it.

.. code-block:: elisp

  (defun my-key-sequences-parser-start (_key-seq)
    "Start parsing on any key sequence."
    t)

  (defun my-key-sequences-parser-stop (_key-seq _state)
    "Stop parsing (i.e., accept) after any key sequence."
    t)

  (defvar my-key-sequences-parser
    (mantra-make-parser "my-key-sequences"
                        #'my-key-sequences-parser-start
                        #'my-key-sequences-parser-stop)
    "A parser to recognize all key sequences.")

The first argument, ``"my-key-sequences"``, is the name of the parser. Whenever the parser recognizes any input, it will publish its parsed output to a shared (dynamic) pub/sub system, using this name as the topic for publication.

The following two function arguments are the stop and abort conditions for the parser. We use named functions instead of lambdas (which would also work) for reasons that will be explained soon.

The first lambda is the predicate for starting parsing. In this case, it always returns true, so this parser starts parsing at the start of each command. The next lambda is the predicate to decide when to stop parsing and accept a result. Since this, too, always returns true, it will always end parsing after each key sequence has been entered.

In other words, this parser will simply publish every key sequence entered.

But the parser can't do anything until it is given input!

Feeding Input to the Parser
~~~~~~~~~~~~~~~~~~~~~~~~~~~

We could manually feed the parser input using ``mantra-feed-parser`` and then have it evaluate the accumulated input using ``mantra-parse``, but a typical use we'd have for such a parser is for it to parse our own keyboard activity in Emacs *implicitly*. To do this, two things are needed:

1. Mantra must be connected to the `Emacs command loop <https://www.gnu.org/software/emacs/manual/html_node/elisp/Command-Overview.html>`_ so that it is notified of all input key sequences. To do this:

.. code-block:: elisp

  (mantra-connect)

2. Our parser (in this case, ``my-key-sequences-parser``) must be subscribed to these key sequences, which Mantra publishes on a generic pub/sub system, under the topic `"mantra-key-sequences"`.

.. code-block:: elisp

  (mantra-subscribe "mantra-key-sequences" my-key-sequences-parser)

Now, Mantra is listening on the Emacs command loop, and it is forwarding all key sequences to our parser. 😎

Subscribing to the Parser
~~~~~~~~~~~~~~~~~~~~~~~~~

Okay, but the main thing you are probably interested in is to actually *do something* with the parsed key sequences. In order to do this, we just need to subscribe to the parser using its name. For instance, the following defines a subscriber (using the ``pubsub`` package used by Mantra to publish results) to the basic parser that simply prints the parsed sequences in human-readable form (using Emacs's built-in ``key-description`` which converts the internal key vector representation into human-readable keys):

.. code-block:: elisp

  (pubsub-subscribe "my-key-sequences"
                    "my-subscriber"
                    (lambda (parsed-keys)
                      (print (key-description parsed-keys))))

You could also use ``(mantra-parser-name my-key-sequences-parser)`` as the topic (first argument) to be extra cautious, but we use the parser's name directly here for simplicity.

Switch to the ``*Messages*`` buffer to see the printed output.

To unsubscribe your printer from the parser:

.. code-block:: elisp

  (pubsub-unsubscribe "mantra-key-sequences"
                      "my-subscriber")

Obviously, this isn't a very useful parser. You can customize the parsing to your specific needs by modifying the ``start``, ``stop``, and ``abort`` conditions, and by using the ``map`` and ``compose`` arguments to the parser, which allow you to define the nature of the parsed result.

Debugging
~~~~~~~~~

If a parser isn't behaving as expected, it can be useful to attach debug logs to each stage of the parsing lifecycle.

Since each parsing stage (i.e., *start*, *stop*, and *abort*) is fulfilled by a function, you can simply use Emacs's built-in way to augment function behavior --- *advice* --- to implement the desired debugging!

As always, with advice in Emacs, it's necessary for the parsing functions to be *named functions* rather than anonymous lambdas, and this is why we avoid lambdas in the definition of ``my-key-sequences-parser`` that we saw earlier. Let's look at how we might use advice to implement debug logs, continuing with our earlier example.

Now, remember that you can use any advice functions you like, but Mantra provides some simple ones that are broadly useful to trace parsing, so we'll use those here.

.. code-block:: elisp

  (require 'mantra-debug)

  (advice-add #'my-key-sequences-parser-start
              :around #'mantra-debug-parser-start)

  (advice-add #'my-key-sequences-parser-stop
              :around #'mantra-debug-parser-stop)

  (advice-add #'my-key-sequences-parser-abort
              :around #'mantra-debug-parser-abort)

Now, open the ``*Messages*`` buffer in a window alongside any buffer where you are doing things, and you should see the debug trace logs appear there for each stage of parsing using the basic parser. When you're satisfied, remove the debugging advice:

.. code-block:: elisp

  (advice-remove #'my-key-sequences-parser-start
                 #'mantra-debug-parser-start)

  (advice-remove #'my-key-sequences-parser-stop
                 #'mantra-debug-parser-stop)

  (advice-remove #'my-key-sequences-parser-abort
                 #'mantra-debug-parser-abort)

As advice is a general way to augment function behavior, you can use this approach to do anything you like in connection with the parsing stages of any particular parser. For instance, you could add additional or alternative conditions for each stage. But this is generally not advisable (so to speak), and it would likely be better to simply write a new parser with the desired functionality rather than override an existing one using advice. Still, knowing this could be useful, as it means parsers used with Mantra are inherently extensible using advice in the same way that Emacs functions are, and with the same caveats.

Troubleshooting
---------------

No Input?
~~~~~~~~~

If you ever write a parser that has an unhandled error in it, Emacs will disable the corresponding listener (in this case, Mantra) on the command loop so that Emacs remains functional. At this point, Mantra parsers will no longer be notified of any activity on the command loop. You might see a sign this has happened in the Messages buffer:

.. code-block:: elisp

  Error in post-command-hook (mantra-post-command-listener): (invalid-function [13])

After fixing the problem, you can reinstate mantra listening on the command loop by calling:

.. code-block:: elisp

  (mantra-connect)

Avoiding Memory Leaks
~~~~~~~~~~~~~~~~~~~~~

If you have unsound accept and abort conditions that may allow the possibility of indefinite parsing and accumulation of state, your parser would take up more and more memory over time, and that could slow down Emacs. If you have reason to believe this cannot happen with your parser and the conditions you've defined, then that's great, and of course, that should be the goal. But to be absolutely sure, as a failsafe, consider defining an upper bound to the size of the state, and aborting if it exceeds that size.

Using Other Input Sources
-------------------------

The example parser above parses key sequences entered on the Emacs command loop. But note that it does so via subscription to a topic on a generic pub/sub system. Indeed, parsers may receive input from *any* source as long as they call ``mantra-feed-parser`` and ``mantra-parse`` (or simply ``mantra-feed-and-parse`` in the most common cases) on your parser, and using the pub/sub backend as an intermediary for the purpose is especially convenient.

Higher Level Parsers
--------------------

As parsers may receive input from any source (typically via pub/sub), in particular, they may receive input from *other parsers*. This is useful if you wish to parse richer structure using the results of lower-level parsers (such as the key sequence parser we wrote above). ``mantra-subscribe`` allows you to describe such pairwise subscription relationships amongst parsers, abstracting the details of feeding the output of one parser as input to the next. You can ``mantra-unsubscribe`` when you're done.

How It Works
------------

Mantra allows you to define "regex"-like patterns on any input data --- for instance, your own keyboard activity --- in terms of start, stop, and abort conditions, together with parsers that transform this keyboard activity (within the context of the surrounding Emacs environment) into arbitrary structured data.

The pattern conditions could be anything, not only based on the key sequence typed. Likewise, the parsers are defined in terms of mapping and composing parsed key sequences within the context of the full surrounding Emacs environment at each step, so that the parsed result could also be just about anything in the context of your activity.

Whenever one of the defined patterns is encountered, the corresponding parser records the structured data (in the earlier example, simply the matching sequence of keystrokes) and publishes it using a basic pub/sub system for additional handling by any subscribers you define. Higher levels of parsing (e.g., "record either buffer or window configuration changes, and only when I'm in such-and-such project path") may be achieved by subscribing to these primitive parsers and publishing fresh events if the desired conditions over these primitive parsers are met.

Mantra is purely syntax, without semantics. It does not bind key sequences to commands or even know what commands parsed sequences may be bound to. The patterns and parsers are defined by you and may be associated with any actions that you see fit to perform, independently of any configured keybindings for these key sequences.

Further Reading
---------------

This package is informed by the perspective developed in `A Vimlike Fluency <https://countvajhula.com/2021/01/21/vim-tip-of-the-day-a-series/>`_, especially:

- `Living the High Life <https://countvajhula.com/2021/02/02/vim-tip-of-the-day-living-the-high-life/>`_
- `Saying More (Macros) <https://countvajhula.com/2021/02/08/vim-tip-of-the-day-saying-more-macros/>`_
- `Going Places <https://countvajhula.com/2021/01/30/vim-tip-of-the-day-going-places/>`_

Non-Ownership
-------------

The freely released, copyright-free work in this repository represents an investment in a better way of doing things called attribution-based economics. Attribution-based economics is based on the simple idea that we gain more by giving more, not by holding on to things that, truly, we could only create because we, in our turn, received from others. As it turns out, an economic system based on attribution -- where those who give more are more empowered -- is significantly more efficient than capitalism while also being stable and fair (unlike capitalism, on both counts), giving it transformative power to elevate the human condition and address the problems that face us today along with a host of others that have been intractable since the beginning. You can help make this a reality by releasing your work in the same way -- freely into the public domain in the simple hope of providing value. Learn more about attribution-based economics at `drym.org <https://drym.org>`_, tell your friends, do your part.
