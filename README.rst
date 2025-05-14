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

Mantra allows you to define "regex"-like patterns on your keyboard activity in terms of start, stop, and abort conditions (which could be anything, not necessarily based on the key sequence typed). Whenever one of these patterns is encountered, the corresponding parser records the matching sequence of keystrokes and publishes it using a basic pub/sub system for additional handling by any subscribers you define.

Mantra is purely syntax, without semantics. It does not bind key sequences to commands or even know what commands parsed sequences may be bound to.

Examples of using this parsed data could include storing and reciting certain mantras in certain contexts, or analyzing keyboard activity for frequent patterns to give you an idea of where to focus your energies toward improving editing efficiency.

Installation
------------

mantra is not on a package archive such as `MELPA <https://melpa.org/>`_ yet, but you can install it using `Straight.el <https://github.com/radian-software/straight.el>`_ (or `Elpaca <https://github.com/progfolio/elpaca>`_) by putting this somewhere in your :code:`.emacs.d`:

.. code-block:: elisp

  (use-package mantra
    :straight
    (mantra
      :type git
      :host github
      :repo "countvajhula/mantra"))

How It Works
------------

Upon each key sequence being entered by you on the `Emacs command loop <https://www.gnu.org/software/emacs/manual/html_node/elisp/Command-Overview.html>`_, a listener (on the pre-command and post-command hooks) does basic "lexing" on this input to ensure that it isn't an empty or otherwise invalid sequence. Then, it notifies every configured repeat ring of the key sequence. Each ring checks its own condition to start recording the sequence. If the condition is met, or if recording is already in progress, then the key sequence is accumulated in a local state variable until either a stop condition is met (in which case the full composite sequence is stored in the ring), or an abort condition is met (in which case the state is cleared and is not stored).

The repeat rings themselves are fixed-size ring data structures, so that they contain a history of length N, with the oldest key sequences being overwritten as new ones are recorded.

Finally, the repeat rings are all stored in a dynamically sized ring of repeat rings that is aware of recency.

Key sequences on each repeat ring may be "repeated" simply by executing them as keyboard macros, via ``execute-kbd-macro``. The function ``mantra-repeat`` simply repeats the macro on the most recently used repeat ring.

Further Reading
---------------

This package generalizes Vim's dot operator and is based on the perspective developed in `A Vimlike Fluency <https://countvajhula.com/2021/01/21/vim-tip-of-the-day-a-series/>`_, especially:

- `Living the High Life <https://countvajhula.com/2021/02/02/vim-tip-of-the-day-living-the-high-life/>`_
- `Saying More (Macros) <https://countvajhula.com/2021/02/08/vim-tip-of-the-day-saying-more-macros/>`_

Non-Ownership
-------------

The freely released, copyright-free work in this repository represents an investment in a better way of doing things called attribution-based economics. Attribution-based economics is based on the simple idea that we gain more by giving more, not by holding on to things that, truly, we could only create because we, in our turn, received from others. As it turns out, an economic system based on attribution -- where those who give more are more empowered -- is significantly more efficient than capitalism while also being stable and fair (unlike capitalism, on both counts), giving it transformative power to elevate the human condition and address the problems that face us today along with a host of others that have been intractable since the beginning. You can help make this a reality by releasing your work in the same way -- freely into the public domain in the simple hope of providing value. Learn more about attribution-based economics at `drym.org <https://drym.org>`_, tell your friends, do your part.
